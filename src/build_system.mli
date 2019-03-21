(** Build rules *)

open! Stdune
open! Import

(** {1 Setup} *)

(** {2 Creation} *)

type hook =
  | Rule_started
  | Rule_completed

(** Initializes the build system. This must be called first. *)
val init
  :  contexts:Context.t list
  -> file_tree:File_tree.t
  -> hook:(hook -> unit)
  -> unit

val reset : unit -> unit

type extra_sub_directories_to_keep =
  | All
  | These of String.Set.t

(** Set the rule generators callback. There must be one callback per
    build context name.

    Each callback is used to generate the rules for a given directory
    in the corresponding build context. It receive the directory for
    which to generate the rules and the split part of the path after
    the build context. It must return an additional list of
    sub-directories to keep. This is in addition to the ones that are
    present in the source tree and the ones that already contain rules.

    It is expected that [f] only generate rules whose targets are
    descendant of [dir]. *)
val set_rule_generators
  :  (dir:Path.t -> string list -> extra_sub_directories_to_keep) String.Map.t
  -> unit

(** All other functions in this section must be called inside the rule generator
    callback. *)

(** {2 Primitive for rule generations} *)

(** Add a rule to the system. This function must be called from the
    [gen_rules] callback. All the target of the rule must be in the
    same directory.

    Assuming that [gen_rules ~dir:a] calls [add_rule r] where [r.dir]
    is [Some b], one of the following assumption must hold:

    - [a] and [b] are the same
    - [gen_rules ~dir:b] calls [load_dir ~dir:a]

    The call to [load_dir ~dir:a] from [gen_rules ~dir:b] declares a
    directory dependency from [b] to [a]. There must be no cyclic
    directory dependencies.  *)
val add_rule : Build_interpret.Rule.t -> unit

(** [prefix_rules t prefix ~f] Runs [f] and adds [prefix] as a
    dependency to all the rules generated by [f] *)
val prefix_rules : (unit, unit) Build.t -> f:(unit -> 'a) -> 'a

(** [eval_pred t ~dir pred ~f] returns the list of files in [dir] that matches
    [pred] to [f]. The list of files includes the list of targets. *)
val eval_pred : dir:Path.t -> string Predicate.t -> string list

(** Returns the set of targets in the given directory. *)
val targets_of : dir:Path.t -> Path.Set.t

(** Load the rules for this directory. *)
val load_dir : dir:Path.t -> unit

(** Stamp files that depends on all files of [dir] with extensions [exts]. *)
val stamp_files_for_files_of : dir:Path.t -> exts:string list -> Path.t list

(** Sets the package this file is part of *)
val set_package : Path.t -> Package.Name.t -> unit

(** Assuming [files] is the list of files in [_build/install] that
    belong to package [pkg], [package_deps t pkg files] is the set of
    direct package dependencies of [package]. *)
val package_deps
  :  Package.Name.t
  -> Path.Set.t
  -> (unit, Package.Name.Set.t) Build.t

(** {2 Aliases} *)

module Alias : sig
  type t

  val pp : t Fmt.t

  val make : string -> dir:Path.t -> t

  val of_user_written_path : loc:Loc.t -> Path.t -> t

  (** The following always holds:

      {[
        make (name t) ~dir:(dir t) = t
      ]}
  *)
  val name : t -> string
  val dir  : t -> Path.t

  val fully_qualified_name : t -> Path.t

  val default     : dir:Path.t -> t
  val runtest     : dir:Path.t -> t
  val install     : dir:Path.t -> t
  val doc         : dir:Path.t -> t
  val private_doc : dir:Path.t -> t
  val lint        : dir:Path.t -> t
  val all         : dir:Path.t -> t
  val check       : dir:Path.t -> t
  val fmt         : dir:Path.t -> t

  (** Alias for all the files in [_build/install] that belong to this
      package *)
  val package_install : context:Context.t -> pkg:Package.Name.t -> t

  (** Return the underlying stamp file *)
  val stamp_file : t -> Path.t

  (** [dep t = Build.path (stamp_file t)] *)
  val dep : t -> ('a, 'a) Build.t

  (** Implements [@@alias] on the command line *)
  val dep_multi_contexts
    :  dir:Path.t
    -> name:string
    -> file_tree:File_tree.t
    -> contexts:string list
    -> (unit, unit) Build.t

  (** Implements [(alias_rec ...)] in dependency specification *)
  val dep_rec
    :  t
    -> loc:Loc.t
    -> file_tree:File_tree.t
    -> (unit, unit) Build.t

  (** Implements [@alias] on the command line *)
  val dep_rec_multi_contexts
    :  dir:Path.t
    -> name:string
    -> file_tree:File_tree.t
    -> contexts:string list
    -> (unit, unit) Build.t

  (** [add_deps store alias ?dyn_deps deps] arrange things so that all
      [dyn_deps] and [deps] are built as part of the build of alias
      [alias]. *)
  val add_deps
    :  t
    -> ?dyn_deps:(unit, Path.Set.t) Build.t
    -> Path.Set.t
    -> unit

  (** [add_action store alias ~stamp action] arrange things so that
      [action] is executed as part of the build of alias
      [alias]. [stamp] is any S-expression that is unique and
      persistent S-expression.
  *)
  val add_action
    :  t
    -> context:Context.t
    -> env:Env.t option
    -> loc:Loc.t option
    -> ?locks:Path.t list
    -> stamp:_
    -> (unit, Action.t) Build.t
    -> unit
end

(** {1 Building} *)

(** All the functions in this section must be called outside the rule generator
    callback. *)

(** Do the actual build *)
val do_build
  :  request:(unit, 'a) Build.t
  -> 'a Fiber.t

(** {2 Other queries} *)

val is_target : Path.t -> bool

(** Return all the library dependencies (as written by the user)
    needed to build this request, by context name *)
val all_lib_deps
  :  request:(unit, unit) Build.t
  -> Lib_deps_info.t Path.Map.t String.Map.t Fiber.t

(** List of all buildable targets *)
val all_targets : unit -> Path.t list

(** Return the set of files that were created in the source tree and
    needs to be deleted *)
val files_in_source_tree_to_delete
  :  unit
  -> Path.Set.t

(** {2 Build rules} *)

(** A fully built rule *)
module Rule : sig
  module Id : sig
    type t
    val to_int : t -> int
    val compare : t -> t -> Ordering.t
  end

  type t =
    { id      : Id.t
    ; dir     : Path.t
    ; deps    : Dep.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }
end

(** Return the list of rules used to build the given targets. If
    [recursive] is [true], return all the rules needed to build the
    given targets and their transitive dependencies. *)
val evaluate_rules
  :  recursive:bool
  -> request:(unit, unit) Build.t
  -> Rule.t list Fiber.t
