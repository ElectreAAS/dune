open Stdune

module Reproducibility_check = struct
  (* CR-someday amokhov: Add [Check_and_repair] to rewrite cache entries if they
     disagree with the check. *)
  type t =
    | Skip
    | Check_with_probability of float
    | Check

  let sample = function
    | Skip -> false
    | Check_with_probability p -> Random.float 1. < p
    | Check -> true
  ;;

  let to_dyn = function
    | Skip -> Dyn.Variant ("Skip", [])
    | Check_with_probability p -> Dyn.Variant ("Check_with_probability", [ Dyn.Float p ])
    | Check -> Dyn.Variant ("Check", [])
  ;;

  let check_with_probability ?loc p =
    let error () =
      User_error.raise
        ?loc
        [ Pp.text "The reproducibility check probability must be in the range [0, 1]." ]
    in
    if p < 0.
    then error ()
    else if p = 0.
    then Skip
    else if p < 1.
    then Check_with_probability p
    else if p = 1.
    then Check
    else error ()
  ;;
end

module Strategy = struct
  type t =
    | Safe
    | Greedy

  let default = Safe
  let all = [ "safe", Safe; "greedy", Greedy ]

  let to_string = function
    | Safe -> "safe"
    | Greedy -> "greedy"
  ;;

  let to_dyn = function
    | Safe -> Dyn.Variant ("Safe", [])
    | Greedy -> Dyn.Variant ("Greedy", [])
  ;;
end

type t =
  | Disabled
  | Enabled of
      { cache_strategy : Strategy.t
      ; storage_mode : Dune_cache_storage.Mode.t
      ; reproducibility_check : Reproducibility_check.t
      }

let to_string = function
  | Disabled -> "disabled"
  | Enabled { cache_strategy; _ } ->
    "enabled with strategy : " ^ Strategy.to_string cache_strategy
;;
