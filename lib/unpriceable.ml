open! Core

type t =
  | Unsupported_date of
      { requested : float
      ; model_horizon : float
      }
  | Path_dependent of { deadline : float }
  | Already_passed of
      { date : float
      ; now : float
      }
  | Off_grid of
      { requested : float
      ; step : float
      }
[@@deriving sexp_of, equal]

let same_time a b = Float.(abs (a - b) <= 1e-12)

let at_rule ~now ~horizon t =
  if same_time t now
  then `Now
  else if Float.(t < now)
  then `Error (Already_passed { date = t; now })
  else if same_time t horizon
  then `Horizon
  else `Error (Unsupported_date { requested = t; model_horizon = horizon })
;;

let anytime_rule ~now ~horizon t =
  if same_time t now
  then `Now
  else if Float.(t < now)
  then `Error (Already_passed { date = t; now })
  else if Float.(t > horizon) && not (same_time t horizon)
  then `Error (Unsupported_date { requested = t; model_horizon = horizon })
  else
    (* The holder can pick any moment between now and the deadline, and the
       best moment depends on the path the price takes. A single-date
       distribution has no path. *)
    `Error (Path_dependent { deadline = t })
;;
