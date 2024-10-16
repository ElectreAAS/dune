open Import
open Action_builder

module Alias_status = struct
  module T = struct
    type t =
      | Defined
      | Not_defined

    let empty : t = Not_defined

    let combine : t -> t -> t =
      fun x y ->
      match x, y with
      | _, Defined | Defined, _ -> Defined
      | Not_defined, Not_defined -> Not_defined
    ;;
  end

  include T
  include Monoid.Make (T)
end

let alias a = dep (Dep.alias a)

module Alias_build_info = struct
  type t =
    { alias_status : Alias_status.t
    ; allowed_build_only_subdirs : Filename.Set.t
    }

  let of_dir_set ~status dirs =
    let allowed_build_only_subdirs =
      match Dir_set.toplevel_subdirs dirs with
      | Infinite -> Filename.Set.empty
      | Finite sub_dirs -> sub_dirs
    in
    { alias_status = status; allowed_build_only_subdirs }
  ;;
end

let register_action_deps : type a. a eval_mode -> Dep.Set.t -> a Dep.Map.t Memo.t =
  fun mode deps ->
  match mode with
  | Eager -> Build_system.build_deps deps
  | Lazy -> Memo.return deps
;;

let dep_on_alias_build_info_if_exists alias =
  of_thunk
    { f =
        (fun mode ->
          let open Memo.O in
          Load_rules.load_dir ~dir:(Path.build (Alias.dir alias))
          >>= function
          | Source _ | External _ ->
            Code_error.raise "Alias in a non-build dir" [ "alias", Alias.to_dyn alias ]
          | Build { aliases; allowed_subdirs; rules_here = _ } ->
            (match Alias.Name.Map.find aliases (Alias.name alias) with
             | None ->
               Memo.return
                 ( Alias_build_info.of_dir_set ~status:Not_defined allowed_subdirs
                 , Dep.Map.empty )
             | Some _ ->
               let deps = Dep.Set.singleton (Dep.alias alias) in
               let+ deps = register_action_deps mode deps in
               Alias_build_info.of_dir_set ~status:Defined allowed_subdirs, deps)
          | Build_under_directory_target _ ->
            Memo.return
              ( Alias_build_info.of_dir_set ~status:Not_defined Dir_set.empty
              , Dep.Map.empty ))
    }
;;

module Alias_rec (Traverse : sig
    val traverse
      :  Path.Build.t
      -> f:(path:Path.Build.t -> Alias_build_info.t t)
      -> Alias_status.t t
  end) =
struct
  open Traverse

  let dep_on_alias_rec name dir =
    let f ~path = dep_on_alias_build_info_if_exists (Alias.make ~dir:path name) in
    traverse dir ~f
  ;;
end
