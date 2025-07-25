open Types
open Exported_types

module Public : sig
  val ping : (unit, unit) Decl.Request.t
  val diagnostics : (unit, Diagnostic.t list) Decl.Request.t
  val shutdown : unit Decl.Notification.t
  val format_dune_file : (Path.t * [ `Contents of string ], string) Decl.Request.t
  val promote : (Path.t, unit) Decl.Request.t
  val build_dir : (unit, Path.t) Decl.Request.t

  module Compound_user_error : sig
    type t =
      { main : User_message.t
      ; related : User_message.t list
      }
  end

  module Build_outcome_with_diagnostics : sig
    type t =
      | Success
      | Failure of Compound_user_error.t list
  end

  module Files_to_promote : sig
    type t =
      | All
      | These of Stdune.Path.Source.t list * (Stdune.Path.Source.t -> unit)
  end

  val promote_many : (Files_to_promote.t, Build_outcome_with_diagnostics.t) Decl.request
end

module Server_side : sig
  val abort : Message.t Decl.Notification.t
  val log : Message.t Decl.Notification.t
end

module Poll : sig
  type 'a t

  val poll : 'a t -> (Id.t, 'a option) Decl.Request.t
  val cancel : 'a t -> Id.t Decl.Notification.t

  module Name : sig
    type t

    val make : string -> t
    val compare : t -> t -> Ordering.t
  end

  val make : Name.t -> (Id.t, 'a option) Decl.Request.gen list -> 'a t
  val name : 'a t -> Name.t
  val progress : Progress.t t
  val diagnostic : Diagnostic.Event.t list t
  val running_jobs : Job.Event.t list t
end
