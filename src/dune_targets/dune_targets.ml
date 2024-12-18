open Import

(* CR-someday amokhov: Most of these records will have [dir = empty]. We might
   want to somehow optimise for the common case, e.g. by switching to a sum type
   with the [Files_only] constructor. It's best not to expose the current
   representation so we can easily change it in future. *)
type t =
  { files : Path.Build.Set.t
  ; dirs : Path.Build.Set.t
  }

module File = struct
  let create file = { files = Path.Build.Set.singleton file; dirs = Path.Build.Set.empty }
end

module Files = struct
  let create files = { files; dirs = Path.Build.Set.empty }
end

let create ~files ~dirs = { files; dirs }
let empty = { files = Path.Build.Set.empty; dirs = Path.Build.Set.empty }

let combine x y =
  { files = Path.Build.Set.union x.files y.files
  ; dirs = Path.Build.Set.union x.dirs y.dirs
  }
;;

let diff t { files; dirs } =
  { files = Path.Build.Set.diff t.files files; dirs = Path.Build.Set.diff t.dirs dirs }
;;

let is_empty { files; dirs } =
  Path.Build.Set.is_empty files && Path.Build.Set.is_empty dirs
;;

let head { files; dirs } =
  match Path.Build.Set.choose files with
  | Some _ as target -> target
  | None -> Path.Build.Set.choose dirs
;;

let to_dyn { files; dirs } =
  Dyn.Record [ "files", Path.Build.Set.to_dyn files; "dirs", Path.Build.Set.to_dyn dirs ]
;;

let all { files; dirs } = Path.Build.Set.to_list files @ Path.Build.Set.to_list dirs

let iter { files; dirs } ~file ~dir =
  Path.Build.Set.iter files ~f:file;
  Path.Build.Set.iter dirs ~f:dir
;;

module Validated = struct
  type unvalidated = t

  (* CR-soon amokhov: Represent these path sets more efficiently, e.g., by a map from the
     parent directory to the corresponding [Filename.Set.t] so that [target_names_in_dir]
     could be implemented without traversing the whole set. *)
  type nonrec t =
    { root : Path.Build.t
    ; files : Filename.Set.t
    ; dirs : Filename.Set.t
    }

  let pp { root; files; dirs } =
    let open Pp.O in
    Pp.hovbox
      (Pp.textf "Validated: root=%S, files=[" (Path.Build.to_string root)
       ++ Pp.concat
            ~sep:(Pp.text "; ")
            (Filename.Set.to_list_map files ~f:(Pp.textf "%S"))
       ++ Pp.text "], dirs=["
       ++ Pp.concat ~sep:(Pp.text "; ") (Filename.Set.to_list_map dirs ~f:(Pp.textf "%S"))
       ++ Pp.char ']')
  ;;

  let iter { root; files; dirs } ~file ~dir =
    Filename.Set.iter files ~f:(fun fn -> file (Path.Build.relative root fn));
    Filename.Set.iter dirs ~f:(fun dn -> dir (Path.Build.relative root dn))
  ;;

  let fold { root; files; dirs } ~init ~file ~dir =
    let acc =
      Filename.Set.fold files ~init ~f:(fun fn -> file (Path.Build.relative root fn))
    in
    Filename.Set.fold dirs ~init:acc ~f:(fun dn -> dir (Path.Build.relative root dn))
  ;;

  let head { root; files; dirs } =
    let name =
      match Filename.Set.choose files with
      | Some name -> name
      | None ->
        (match Filename.Set.choose dirs with
         | Some name -> name
         | None -> assert false)
    in
    Path.Build.relative root name
  ;;

  let unvalidate t : unvalidated =
    { files =
        Path.Build.Set.of_listing ~dir:t.root ~filenames:(Filename.Set.to_list t.files)
    ; dirs =
        Path.Build.Set.of_listing ~dir:t.root ~filenames:(Filename.Set.to_list t.dirs)
    }
  ;;

  let to_dyn { root; files; dirs } =
    Dyn.Record
      [ "root", Path.Build.to_dyn root
      ; "files", Filename.Set.to_dyn files
      ; "dirs", Filename.Set.to_dyn dirs
      ]
  ;;

  let to_trace_args { root; files; dirs } =
    let mkset s xs =
      if Filename.Set.is_empty xs
      then []
      else
        [ ( s
          , `List
              (Filename.Set.to_list_map xs ~f:(fun x ->
                 `String (Path.Build.relative root x |> Path.Build.to_string))) )
        ]
    in
    mkset "target_files" files @ mkset "target_dirs" dirs
  ;;
end

module Validation_result = struct
  type t =
    | Valid of Validated.t
    | No_targets
    | Inconsistent_parent_dir
    | File_and_directory_target_with_the_same_name of Path.Build.t
end

let validate { files; dirs } =
  let add_file (t : Validated.t) name =
    Validation_result.Valid { t with files = Filename.Set.add t.files name }
  in
  let add_dir (t : Validated.t) name =
    if Filename.Set.mem t.files name
    then
      Validation_result.File_and_directory_target_with_the_same_name
        (Path.Build.relative t.root name)
    else Valid { t with dirs = Filename.Set.add t.dirs name }
  in
  let build (init : Validation_result.t) ~paths ~f =
    Path.Build.Set.fold paths ~init ~f:(fun path res ->
      let parent = Path.Build.parent_exn path in
      let name = Path.Build.basename path in
      match res with
      | No_targets ->
        let t =
          { Validated.root = parent
          ; files = Filename.Set.empty
          ; dirs = Filename.Set.empty
          }
        in
        f t name
      | Valid t when Path.Build.equal t.root parent -> f t name
      | Valid _ -> Inconsistent_parent_dir
      | (Inconsistent_parent_dir | File_and_directory_target_with_the_same_name _) as res
        -> res)
  in
  build No_targets ~paths:files ~f:add_file |> build ~paths:dirs ~f:add_dir
;;

module Produced = struct
  (* CR-someday amokhov: A hierarchical representation of the produced file
     trees may be better. It would allow for hierarchical traversals and reduce
     the number of internal invariants. *)

  (** All file names and directory names are relative to the root (['a t]). *)
  type 'a dir_contents =
    { files : 'a Filename.Map.t (* mapping file name -> 'a *)
    ; subdirs :
        'a dir_contents Filename.Map.t (* mapping directory name -> 'a dir_contents *)
    }

  let is_empty_dir_conts { files; subdirs } =
    Filename.Map.is_empty files && Filename.Map.is_empty subdirs
  ;;

  let rec pp_dir_conts ?(payload_printer = fun _ -> Pp.char '?') contents =
    if is_empty_dir_conts contents
    then Pp.text "<empty dir_contents>"
    else (
      let { files; subdirs } = contents in
      let open Pp.O in
      Pp.text "{ "
      ++ Pp.hovbox
           (Pp.text "Files: ("
            ++ Pp.box
                 (Pp.concat
                    ~sep:(Pp.text ", ")
                    (Filename.Map.to_list_map files ~f:(fun name payload ->
                       Pp.textf "%S -> " name ++ payload_printer payload)))
            ++ Pp.text ");"
            ++ Pp.space
            ++ Pp.hovbox
                 (Pp.text "Subdirs: ("
                  ++ Pp.concat
                       ~sep:(Pp.text ", ")
                       (Filename.Map.to_list_map subdirs ~f:(fun name sub ->
                          Pp.textf "%S -> " name ++ pp_dir_conts ~payload_printer sub))
                  ++ Pp.text ")"))
      ++ Pp.char '}')
  ;;

  type 'a t =
    { root : Path.Build.t
    ; contents : 'a dir_contents
    }

  let pp ?(payload_printer = fun _ -> Pp.char '?') { root; contents } =
    let open Pp.O in
    Pp.hovbox
      (Pp.textf "root=%S, contents=" (Path.Build.to_string root)
       ++ pp_dir_conts ~payload_printer contents)
  ;;

  module Error = struct
    type t =
      | Missing_dir of Path.Build.t
      | Empty_dir of Path.Build.t
      | Unreadable_dir of Path.Build.t * Unix_error.Detailed.t
      | Unsupported_file of Path.Build.t * File_kind.t

    let message = function
      | Missing_dir dir ->
        [ Pp.textf
            "Rule failed to produce directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ]
      | Empty_dir dir ->
        [ Pp.textf
            "Rule produced directory %S that contains no files nor non-empty \
             subdirectories"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ]
      | Unreadable_dir (dir, (unix_error, _, _)) ->
        (* CR-soon amokhov: This case is untested. *)
        [ Pp.textf
            "Rule produced unreadable directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ; Pp.verbatim (Unix.error_message unix_error)
        ]
      | Unsupported_file (file, kind) ->
        (* CR-soon amokhov: This case is untested. *)
        [ Pp.textf
            "Rule produced file %S with unrecognised kind %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn file
             |> Path.Source.to_string_maybe_quoted)
            (File_kind.to_string kind)
        ]
    ;;

    let to_string_hum = function
      | Missing_dir _ -> "missing directory"
      | Empty_dir _ -> "empty directory"
      | Unreadable_dir (_, unix_error) -> Unix_error.Detailed.to_string_hum unix_error
      | Unsupported_file _ -> "unsupported file kind"
    ;;
  end

  let debug_create = false
  let debug_consume = false
  let debug_out = false

  let rec merge_contents c1 c2 =
    let files =
      Filename.Map.union c1.files c2.files ~f:(fun _ p1 p2 ->
        assert (Poly.equal p1 p2);
        Some p1)
    in
    let subdirs =
      Filename.Map.union c1.subdirs c2.subdirs ~f:(fun _ s1 s2 ->
        Some (merge_contents s1 s2))
    in
    { files; subdirs }
  ;;

  let merge t1 t2 =
    if not (Path.Build.equal t1.root t2.root)
    then Code_error.raise "Can't merge two targets with different roots" [];
    let contents = merge_contents t1.contents t2.contents in
    { root = t1.root; contents }
  ;;

  (** The call sites ensure that [dir = Path.Build.append_local validated.root local].
      No need for [local] actually... *)
  let rec contents_of_dir ~file_f (dir : Path.Build.t) : ('a dir_contents, Error.t) result
    =
    let open Result.O in
    let init = { files = Filename.Map.empty; subdirs = Filename.Map.empty } in
    match Path.readdir_unsorted_with_kinds (Path.build dir) with
    | Error (Unix.ENOENT, _, _) -> Error (Missing_dir dir)
    | Error e -> Error (Unreadable_dir (dir, e))
    | Ok dir_contents ->
      let+ results =
        Result.List.fold_left
          dir_contents
          ~init
          ~f:(fun { files; subdirs } (name, kind) ->
            match (kind : File_kind.t) with
            | S_LNK | S_REG ->
              let files =
                match file_f (Path.Local.relative (Path.Build.local dir) name) with
                | Some payload -> Filename.Map.add_exn files name payload
                | None -> files
              in
              Ok { files; subdirs }
            | S_DIR ->
              let+ subdirs_contents =
                contents_of_dir ~file_f (Path.Build.relative dir name)
              in
              { files; subdirs = Filename.Map.add_exn subdirs name subdirs_contents }
            | _ -> Error (Unsupported_file (Path.Build.relative dir name, kind)))
      in
      (if debug_create
       then
         let open Pp.O in
         Pp.to_fmt
           Format.std_formatter
           (Pp.paragraphf "In contents_of_dir %S =>" (Path.Build.to_string dir)
            ++ pp_dir_conts results
            ++ Pp.space));
      results
  ;;

  let of_validated (validated : Validated.t) =
    let open Result.O in
    (* We assume here that [dir_name] is either a child of [root], or that we're ok with having [root/a/b] but not [root/a]. *)
    let aggregate_dir { root; contents = { files; subdirs } } dir_name =
      let dir = Path.Build.relative root dir_name in
      let* new_contents = contents_of_dir ~file_f:(fun _ -> Some ()) dir in
      if Filename.Map.is_empty new_contents.files
         && Filename.Map.is_empty new_contents.subdirs
      then Error (Empty_dir dir)
      else (
        let contents =
          { files; subdirs = Filename.Map.add_exn subdirs dir_name new_contents }
        in
        Ok { root; contents })
    in
    let rooted_files = Filename.Set.to_map validated.files ~f:(Fun.const ()) in
    let+ result =
      Filename.Set.to_list validated.dirs
      |> Result.List.fold_left
           ~init:
             { root = validated.root
             ; contents = { files = rooted_files; subdirs = Filename.Map.empty }
             }
           ~f:aggregate_dir
    in
    (if debug_create
     then
       let open Pp.O in
       Pp.to_fmt
         Format.std_formatter
         (Pp.hovbox
            (Pp.paragraph "In of_validated("
             ++ Pp.cut
             ++ Validated.pp validated
             ++ Pp.text ") => "
             ++ pp ~payload_printer:(fun () -> Pp.text "()") result)
          ++ Pp.text "\n\n"));
    result
  ;;

  (** For each file, not only do we add it to the [t] in proper place, we also add the rest of the contents of the directories. *)
  let of_files_with_neighbours root (file_list : 'a Path.Local.Map.t) =
    let init = { files = Filename.Map.empty; subdirs = Filename.Map.empty } in
    let contents =
      Path.Local.Map.foldi file_list ~init ~f:(fun file payload contents ->
        match Path.Local.explode file with
        | [] -> failwith "TODO"
        | [ at_root ] ->
          { contents with files = Filename.Map.add_exn contents.files at_root payload }
        | parent_dir :: _rest ->
          let dir = Path.Build.relative root parent_dir in
          (match
             contents_of_dir
               ~file_f:(fun name ->
                 if debug_create
                 then
                   Pp.to_fmt
                     Format.std_formatter
                     (Pp.paragraphf "Examining %S...\n" (Path.Local.to_string name));
                 if Path.Local.equal name file then Some payload else None)
               dir
           with
           | Ok subdirs -> merge_contents subdirs contents
           | Error _ -> contents))
    in
    (if debug_create
     then
       let open Pp.O in
       Pp.to_fmt
         Format.std_formatter
         (Pp.text "In of_files_w_n ("
          ++ Pp.hovbox
               (Pp.concat
                  ~sep:(Pp.text ", ")
                  (Path.Local.Map.to_list_map file_list ~f:(fun file_name _payload ->
                     Pp.textf "%S -> ?" (Path.Local.to_string file_name))))
          ++ Pp.text ") => "
          ++ pp_dir_conts contents));
    { root; contents }
  ;;

  let of_files root file_list =
    let rec aux payload { files; subdirs } = function
      | [] ->
        Code_error.raise
          "I've been hoisted by my own petard! (path.explode)"
          [ "file_list", Path.Local.Map.to_dyn Dyn.opaque file_list ]
      | [ final ] -> { files = Filename.Map.add_exn files final payload; subdirs }
      | parent :: rest ->
        let subdirs =
          Filename.Map.update subdirs parent ~f:(fun contents_opt ->
            Some
              (aux
                 payload
                 (Option.value
                    contents_opt
                    ~default:{ files = Filename.Map.empty; subdirs = Filename.Map.empty })
                 rest))
        in
        { files; subdirs }
    in
    let init = { files = Filename.Map.empty; subdirs = Filename.Map.empty } in
    let contents =
      Path.Local.Map.foldi file_list ~init ~f:(fun file payload contents ->
        let parent = Path.Local.parent_exn file in
        if Path.Local.is_root parent
        then
          { contents with
            files =
              Filename.Map.add_exn contents.files (Path.Local.to_string file) payload
          }
        else aux payload contents (Path.Local.explode file))
    in
    (if debug_create
     then
       let open Pp.O in
       Pp.to_fmt
         Format.std_formatter
         (Pp.text "In of_files ("
          ++ Pp.hovbox
               (Pp.concat
                  ~sep:(Pp.text ", ")
                  (Path.Local.Map.to_list_map file_list ~f:(fun file_name _payload ->
                     Pp.textf "%S -> ?" (Path.Local.to_string file_name))))
          ++ Pp.text ") => "
          ++ pp { root; contents }));
    { root; contents }
  ;;

  let all_files_seq { contents; root = _ } =
    let rec aux path { files; subdirs } =
      Seq.append
        (Filename.Map.to_seq files
         |> Seq.map ~f:(fun (file_name, payload) ->
           let file = Path.Local.relative path file_name in
           (if debug_consume
            then
              let open Pp.O in
              Pp.to_fmt
                Format.std_formatter
                (Pp.paragraphf "[FileSeq %S]" (Path.Local.to_string file) ++ Pp.space));
           file, payload))
        (Seq.concat
           (Filename.Map.to_seq subdirs
            |> Seq.map ~f:(fun (dir_name, subdir_contents) ->
              aux (Path.Local.relative path dir_name) subdir_contents)))
    in
    aux Path.Local.root contents
  ;;

  let all_dirs_seq { root = _; contents } =
    let rec aux path { subdirs; files = _ } =
      Seq.concat
        (Filename.Map.to_seq subdirs
         |> Seq.map ~f:(fun (subdir_name, subdir_contents) ->
           let subdir = Path.Local.relative path subdir_name in
           (if debug_consume
            then
              let open Pp.O in
              Pp.to_fmt
                Format.std_formatter
                (Pp.paragraphf "[DirSeq %S] " (Path.Local.to_string subdir) ++ Pp.space));
           Seq.cons (subdir, subdir_contents) (aux subdir subdir_contents)))
    in
    aux Path.Local.root contents
  ;;

  let find ({ root; contents } as r) file =
    let open Option.O in
    let rec find_aux path { files; subdirs } = function
      | [] ->
        Code_error.raise
          "I've been hoisted by my own petard! (path.explode)"
          [ "file", Path.Build.to_dyn file ]
      | [ final ] -> Filename.Map.find files final
      | parent :: rest ->
        let path = Path.Local.relative path parent in
        let* subdir = Filename.Map.find subdirs parent in
        find_aux path subdir rest
    in
    let root = Path.Build.local root in
    let* path = Path.Local.descendant (Path.Build.local file) ~of_:root in
    let result = find_aux root contents (Path.Local.explode path) in
    (let open Pp.O in
     if debug_consume
     then
       Pp.to_fmt
         Format.std_formatter
         (Pp.paragraphf "In find (%S):" (Path.Build.to_string file)
          ++ Pp.space
          ++ pp r
          ++ Pp.text " => "
          ++ Pp.paragraph
               (if Option.is_some result then "found something!" else "found nothing!")
          ++ Pp.text "\n\n"));
    result
  ;;

  let mem t path = Option.is_some (find t path)

  let find_dir ({ root; contents } as r) dir =
    let open Option.O in
    let rec find_dir_aux path { subdirs; files = _ } = function
      | [] ->
        Code_error.raise
          "I've been hoisted by my own petard! (path.explode)"
          [ "dir", Path.Build.to_dyn dir ]
      | [ final ] ->
        let+ subdir =
          Filename.Map.find subdirs final
          (* (Path.Local.relative path final) *)
        in
        subdir.files
      | parent :: rest ->
        let path = Path.Local.relative path parent in
        let* subdir = Filename.Map.find subdirs parent in
        find_dir_aux path subdir rest
    in
    let root = Path.Build.local root in
    let* path = Path.Local.descendant (Path.Build.local dir) ~of_:root in
    let result = find_dir_aux root contents (Path.Local.explode path) in
    (let open Pp.O in
     if debug_consume
     then
       Pp.to_fmt
         Format.std_formatter
         (Pp.paragraphf "In find_dir (%S): " (Path.Build.to_string dir)
          ++ pp r
          ++ Pp.text " => "
          ++ Pp.paragraph
               (if Option.is_some result then "found something!" else "found nothing!")
          ++ Pp.text "\n\n"));
    result
  ;;

  let mem_dir t path = Option.is_some (find_dir t path)

  let equal
    { root = root1; contents = contents1 }
    { root = root2; contents = contents2 }
    ~equal
    =
    let rec eq_aux { files = files1; subdirs = dirs1 } { files = files2; subdirs = dirs2 }
      =
      Filename.Map.equal files1 files2 ~equal
      && Filename.Map.equal dirs1 dirs2 ~equal:eq_aux
    in
    Path.Build.equal root1 root2 && eq_aux contents1 contents2
  ;;

  let exists { root = _; contents } ~f =
    let rec aux { files; subdirs } =
      Filename.Map.exists files ~f || Filename.Map.exists subdirs ~f:aux
    in
    aux contents
  ;;

  let foldi { contents; root = _ } ~init ~f =
    let rec aux path { files; subdirs } acc =
      let acc =
        Filename.Map.foldi files ~init:acc ~f:(fun file_name acc ->
          let file = Path.Local.relative path file_name in
          (if debug_consume
           then
             let open Pp.O in
             Pp.to_fmt
               Format.std_formatter
               (Pp.paragraphf "[Foldi %S] " (Path.Local.to_string file) ++ Pp.space));
          f file acc)
      in
      Filename.Map.foldi subdirs ~init:acc ~f:(fun dir_name ->
        aux (Path.Local.relative path dir_name))
    in
    aux Path.Local.root contents init
  ;;

  let iteri { contents; root = _ } ~f ~d =
    let rec aux path { files; subdirs } =
      Filename.Map.iteri files ~f:(fun file_name payload ->
        let file = Path.Local.relative path file_name in
        (if debug_consume
         then
           let open Pp.O in
           Pp.to_fmt
             Format.std_formatter
             (Pp.paragraphf "[Iteri F %S]" (Path.Local.to_string file) ++ Pp.space));
        f file payload);
      Filename.Map.iteri subdirs ~f:(fun dir_name dir_contents ->
        let dir = Path.Local.relative path dir_name in
        (if debug_consume
         then
           let open Pp.O in
           Pp.to_fmt
             Format.std_formatter
             (Pp.paragraphf "[Iteri D %S]" (Path.Local.to_string dir) ++ Pp.space));
        d dir dir_contents;
        (* Depth-first traversal here. *)
        aux dir dir_contents)
    in
    aux Path.Local.root contents
  ;;

  let iter_files t ~f = iteri t ~f ~d:(fun _ _ -> ())
  let iter_dirs t ~f = iteri t ~f:(fun _ _ -> ()) ~d:f

  module Path_traversal = Fiber.Make_parallel_map (Path.Local.Map)
  module Filename_traversal = Fiber.Make_parallel_map (String.Map)

  let parallel_map { root; contents } ~f =
    let open Fiber.O in
    let rec aux path { files; subdirs } =
      let+ files, subdirs =
        Fiber.fork_and_join
          (fun () ->
            Filename_traversal.parallel_map files ~f:(fun file_name ->
              let file = Path.Local.relative path file_name in
              (if debug_consume
               then
                 let open Pp.O in
                 Pp.to_fmt
                   Format.std_formatter
                   (Pp.paragraphf "[Paramap %S]" (Path.Local.to_string file) ++ Pp.space));
              f file))
          (fun () ->
            Filename_traversal.parallel_map subdirs ~f:(fun dir_name ->
              aux (Path.Local.relative path dir_name)))
      in
      { files; subdirs }
    in
    let+ contents = aux Path.Local.root contents in
    { root; contents }
  ;;

  let digest { root = _; contents } =
    let rec all_digests _ { files; subdirs } =
      let ffiles = Filename.Map.values files in
      List.concat (ffiles :: Filename.Map.to_list_map subdirs ~f:all_digests)
    in
    Digest.generic (all_digests "ignored" contents)
  ;;

  exception Short_circuit

  let map_with_errors
    { root; contents }
    ~all_errors
    ~(f : Path.Build.t -> 'a -> ('b, 'e) result)
    =
    let errors = ref [] in
    let f path a =
      match f path a with
      | Ok s -> Some s
      | Error e ->
        errors := (path, e) :: !errors;
        if all_errors then None else raise_notrace Short_circuit
    in
    let rec aux path { files; subdirs } =
      let files =
        Filename.Map.filter_mapi files ~f:(fun file ->
          (if debug_consume
           then
             let open Pp.O in
             Pp.to_fmt
               Format.std_formatter
               (Pp.paragraphf
                  "[Map/w/E %S]"
                  (Path.Build.to_string (Path.Build.relative path file))
                ++ Pp.space));
          f (Path.Build.relative path file))
      in
      let subdirs =
        Filename.Map.mapi subdirs ~f:(fun dir subdirs_contents ->
          let dir = Path.Build.relative path dir in
          aux dir subdirs_contents)
      in
      { files; subdirs }
    in
    let result =
      try { root; contents = aux root contents } with
      | Short_circuit ->
        { root; contents = { files = Filename.Map.empty; subdirs = Filename.Map.empty } }
    in
    match Nonempty_list.of_list !errors with
    | None -> Ok result
    | Some list -> Error list
  ;;

  let to_dyn { root; contents } =
    let rec aux { files; subdirs } =
      Dyn.record
        [ "files", Filename.Map.to_dyn Dyn.opaque files
        ; "dirs", Filename.Map.to_dyn aux subdirs
        ]
    in
    Dyn.record [ "root", Path.Build.to_dyn root; "contents", aux contents ]
  ;;
end
