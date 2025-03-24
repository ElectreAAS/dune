open Import
open Dune_cache_storage.Layout
open Fiber.O

module Store_artifacts_result = struct
  type t =
    | Stored of Digest.t Targets.Produced.t
    | Already_present of Digest.t Targets.Produced.t
    | Error of exn
    | Will_not_store_due_to_non_determinism of Sexp.t

  let of_store_result ~artifacts t =
    match (t : Store_result.t) with
    | Stored -> Stored artifacts
    | Already_present -> Already_present artifacts
    | Error exn -> Error exn
    | Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
  ;;

  let bind t ~artifacts ~f =
    match (t : Store_result.t) with
    | Stored -> f artifacts
    | Already_present -> f artifacts
    | Error exn -> Error exn
    | Will_not_store_due_to_non_determinism details ->
      Will_not_store_due_to_non_determinism details
  ;;
end

module Target = struct
  type t = { permissions : Unix.file_perm }

  let create path =
    match Path.Build.lstat path with
    | { Unix.st_kind = Unix.S_REG; st_perm; _ } ->
      let mode = Path.Permissions.(remove write st_perm) in
      Path.Build.chmod path ~mode;
      Some { permissions = mode }
    | { Unix.st_kind = Unix.S_DIR; st_perm; _ } ->
      (* Adding "executable" permissions to directories mean we can traverse them. *)
      let mode = Path.Permissions.(add execute st_perm) in
      Path.Build.chmod path ~mode;
      (* the value of [permissions] here is ignored, but [Some] is meaningful. *)
      Some { permissions = mode }
    | (exception Unix.Unix_error _) | _ -> None
  ;;
end

(* This function is like [Unix.link] but handles the "Too many links" error by
   creating a copy of the [src] in a temporary directory, then atomically
   replacing the [src] with the copy, and finally creating the requested [dst]
   by calling [Unix.link src dst] again.

   We hit the "Too many links" error because we store a lot of empty files in
   the cache, which all get deduplicated into the same cache entry. This
   function essentially deletes the "overlinked" entry from the cache, creating
   a fresh copy with the 0 link count. This leads to some duplication but it's
   negligible: we might store the empty file several times across all workspaces
   instead of just storing it once.

   If you need to debug this function, you can trigger the "Too many links"
   error by running [for i in {1..100000}; do ln $file tmp/$i; done], where the
   [$file] is the shared cache entry for the empty file. After that, no more
   hard links on [$file] will be allowed, triggering the [EMLINK] code path. *)
let link_even_if_there_are_too_many_links_already ~src ~dst =
  try Path.link src dst with
  | Unix.Unix_error (Unix.EMLINK, _, _) ->
    Temp.with_temp_file ~dir:temp_dir ~prefix:"dune" ~suffix:"copy" ~f:(function
      | Error e -> raise e
      | Ok temp_file ->
        Io.copy_file ~src ~dst:temp_file ();
        (* This replaces [src], which has too many links already, with a fresh
           copy we've just created in the [temp_file]. *)
        Path.rename temp_file src;
        (* This should now succeed. *)
        Path.link src dst)
;;

module Artifacts = struct
  include Dune_cache_storage.Artifacts

  let store_metadata
        ~mode
        ~metadata
        ~rule_digest
        (artifacts : (Digest.t * Unix.file_perm) Targets.Produced.t)
    =
    let entries, artifacts =
      Targets.Produced.magic_map artifacts ~init:[] ~f:(fun target digest_and_perm acc ->
        let digest, permissions =
          match digest_and_perm with
          | Some (digest, permissions) -> Some digest, permissions
          | None ->
            ( None
            , (* FIXME: what permissions do we give directories here? Does it matter? *) 0
            )
        in
        { Metadata_entry.path = Path.Local.to_string target; digest; permissions } :: acc)
    in
    Metadata_file.store ~mode { metadata; entries } ~rule_digest
    |> Store_artifacts_result.of_store_result ~artifacts
  ;;

  (* Step I of [store_skipping_metadata].

     If any of the targets couldn't be stored in the temporary directory, then
     the result is [Error] with the corresponding exception. Otherwise, the
     result is [Ok ()]. *)
  let store_targets_to ~temp_dir ~(targets : _ Targets.Produced.t) ~mode : unit Or_exn.t =
    let portable_hardlink_or_copy =
      match (mode : Dune_cache_storage.Mode.t) with
      | Hardlink -> Io.portable_hardlink
      | Copy -> fun ~src ~dst -> Io.copy_file ~src ~dst ()
    in
    Result.try_with (fun () ->
      (* CR-someday rleshchinskiy: We recreate the directory structure here but it might be
         simpler to just use file digests instead of file names and no subdirectories. *)
      (* The comment above seems outdated wrt. 'no subdirectories'... *)
      Targets.Produced.iteri
        targets
        ~d:(fun dir -> Path.mkdir_p (Path.append_local temp_dir dir))
        ~f:(fun file _ ->
          let path_in_build_dir =
            Path.build (Path.Build.append_local targets.root file)
          in
          let path_in_temp_dir = Path.append_local temp_dir file in
          portable_hardlink_or_copy ~src:path_in_build_dir ~dst:path_in_temp_dir))
  ;;

  (* Step II of [store_skipping_metadata].

     Computing digests can be slow, so we do that in parallel. *)
  let compute_digests_and_perm ~temp_dir ~targets ~compute_digest
    : (Digest.t * Unix.file_perm) Targets.Produced.t Or_exn.t Fiber.t
    =
    let open Fiber.O in
    Fiber.collect_errors (fun () ->
      Targets.Produced.parallel_map targets ~f:(fun path { Target.permissions } ->
        let file = Path.append_local temp_dir path in
        let executable = Path.Permissions.(test execute permissions) in
        let+ digest = compute_digest ~executable file in
        digest, permissions))
    >>| Result.map_error ~f:(function
      | exn :: _ -> exn.Exn_with_backtrace.exn
      | [] -> assert false)
  ;;

  (* Step III of [store_skipping_metadata]. *)
  let store_to_cache_from ~temp_dir ~mode artifacts =
    Targets.Produced.foldi
      artifacts
      ~init:Store_result.empty
      ~f:(fun target digest_and_perm results ->
        match digest_and_perm with
        | None ->
          (* No digest means [target] is a directory, simply ignore it. *)
          results
        | Some (file_digest, file_perm) ->
          let path_in_temp_dir = Path.append_local temp_dir target in
          let path_in_cache = file_path ~file_digest in
          let store_using_hardlinks () =
            match
              Dune_cache_storage.Util.Optimistically.link
                ~src:path_in_temp_dir
                ~dst:path_in_cache;
              Format.printf "Before chmod 1a: %o@." file_perm;
              Path.chmod path_in_cache ~mode:file_perm;
              Format.printf "chmod 1a went through@."
            with
            | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
              (* We end up here if the cache already contains an entry for this
                 artifact. We deduplicate by keeping only one copy, in the
                 cache. *)
              let path_in_build_dir =
                Path.build (Path.Build.append_local artifacts.root target)
              in
              (match
                 Path.unlink_no_err path_in_temp_dir;
                 (* At first, we deduplicate the temporary file. Doing this
                    intermediate step allows us to keep the original target in case
                    the below link step fails. This might happen if the trimmer has
                    just deleted [path_in_cache]. In this rare case, this function
                    fails with an [Error], and so we might end up with some
                    duplicates in the workspace. *)
                 link_even_if_there_are_too_many_links_already
                   ~src:path_in_cache
                   ~dst:path_in_temp_dir;
                 (* Now we can simply rename the temporary file into the target,
                    knowing that the original target remains in place if the
                    renaming fails.

                    One curious case to think about is if the file in the cache
                    happens to have the same inode as the file in the workspace. In
                    that case this deduplication should be a no-op, but the
                    [rename] operation has a quirk where [path_in_temp_dir] can
                    remain on disk. This is not a problem because we clean the
                    temporary directory later. *)
                 Path.rename path_in_temp_dir path_in_build_dir
               with
               | exception e -> Store_result.Error e
               | () -> Already_present)
            | exception e -> Error e
            | () -> Stored
          in
          let store_using_test_and_rename () =
            (* CR-someday amokhov: There is a race here. If [path_in_cache] is
               created after [Path.exists] but before [Path.rename], it will be
               silently overwritten. Find a good way to avoid this race. *)
            match Path.exists path_in_cache with
            | true -> Store_result.Already_present
            | false ->
              (match
                 Dune_cache_storage.Util.Optimistically.rename
                   ~src:path_in_temp_dir
                   ~dst:path_in_cache;
                 Format.printf "Before chmod 1b: %o@." file_perm;
                 Path.chmod path_in_temp_dir ~mode:file_perm;
                 Format.printf "chmod 1b went through@."
               with
               | exception e -> Error e
               | () -> Stored)
          in
          let result =
            match (mode : Dune_cache_storage.Mode.t) with
            | Hardlink -> store_using_hardlinks ()
            | Copy -> store_using_test_and_rename ()
          in
          Store_result.combine results result)
  ;;

  let store_skipping_metadata
        ~mode
        ~targets
        ~compute_digest
        (* : Store_artifacts_result.t Fiber.t *)
    =
    Dune_cache_storage.with_temp_dir ~suffix:"artifacts" (function
      | Error exn -> Fiber.return (Store_result.Error exn, Targets.Produced.empty)
      | Ok temp_dir ->
        (match store_targets_to ~temp_dir ~targets ~mode with
         | Error exn -> Fiber.return (Store_result.Error exn, Targets.Produced.empty)
         | Ok () ->
           compute_digests_and_perm ~temp_dir ~targets ~compute_digest
           >>| (function
            | Error exn -> Store_result.Error exn, Targets.Produced.empty
            | Ok artifacts ->
              let result = store_to_cache_from ~temp_dir ~mode artifacts in
              result, artifacts)))
  ;;

  let store ~mode ~rule_digest ~compute_digest targets : Store_artifacts_result.t Fiber.t =
    let+ result, artifacts = store_skipping_metadata ~mode ~targets ~compute_digest in
    Store_artifacts_result.bind result ~artifacts ~f:(fun artifacts ->
      store_metadata ~mode ~rule_digest ~metadata:[] artifacts)
  ;;

  module File_restore = struct
    exception E of Digest.t Targets.Produced.t Restore_result.t

    module Unwind : sig
      type t

      val make : unit -> t
      val push : t -> (unit -> unit) -> unit
      val unwind : t -> unit
    end = struct
      type t = (unit -> unit) list ref

      let make () = ref []
      let push t f = t := f :: !t

      let unwind t =
        List.iter !t ~f:(fun f ->
          try f () with
          | _ -> ());
        t := []
      ;;
    end

    let hardlink ~src ~dst =
      try link_even_if_there_are_too_many_links_already ~src ~dst with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> raise_notrace (E Not_found_in_cache)
      | exn -> raise_notrace (E (Error exn))
    ;;

    let copy ~src ~dst =
      try Io.copy_file ~src ~dst () with
      | Sys_error _ -> raise_notrace (E Not_found_in_cache)
    ;;

    let create_all_or_none
          (mode : Dune_cache_storage.Mode.t)
          (artifacts : (Dune_digest.t * Unix.file_perm) Targets.Produced.t)
      =
      let unwind = Unwind.make () in
      let rec mk_dir (dir : Path.Local.t) =
        (match Path.Local.parent dir with
         | Some parent when not (Path.Local.is_root parent) -> mk_dir parent
         | Some _ | None -> ());
        let path = Path.build (Path.Build.append_local artifacts.root dir) in
        if not (Path.exists path)
        then (
          Path.mkdir_p path;
          Unwind.push unwind (fun () -> Path.rmdir path))
      in
      let mk_file file (file_digest, perm) =
        let target = Path.Build.append_local artifacts.root file in
        let dst = Path.build target in
        let src = file_path ~file_digest in
        (match mode with
         | Hardlink -> hardlink ~src ~dst
         | Copy -> copy ~src ~dst);
        Format.printf "Before chmod 2: %o@." perm;
        Path.chmod dst ~mode:perm;
        Unwind.push unwind (fun () -> Path.Build.unlink_no_err target);
        file_digest
      in
      try Targets.Produced.map ~d:mk_dir ~f:mk_file artifacts with
      | exn ->
        Unwind.unwind unwind;
        reraise exn
    ;;
  end

  let restore ~mode ~rule_digest ~target_dir =
    Restore_result.bind (list ~rule_digest) ~f:(fun (entries : Metadata_entry.t list) ->
      let artifacts =
        Path.Local.Map.of_list_map_exn
          entries
          ~f:(fun { Metadata_entry.path; digest; permissions } ->
            Path.Local.of_string path, Option.map digest ~f:(fun d -> d, permissions))
        |> Targets.Produced.of_files target_dir
      in
      try
        let artifacts = File_restore.create_all_or_none mode artifacts in
        Restored artifacts
      with
      | File_restore.E result ->
        (* If [result] is [Not_found_in_cache] then one of the entries mentioned in
           the metadata is missing. The trimmer will eventually delete such "broken"
           metadata, so it is reasonable to consider that this [rule_digest] is not
           found in the cache. *)
        result)
  ;;
end

let store_artifacts = Artifacts.store
let restore_artifacts = Artifacts.restore
