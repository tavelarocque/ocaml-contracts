(** Why a contract can't be priced, and the date rules every pricer shares.

    A pricer that works from a distribution of the price on one date can only
    value what happens on that date. These rules decide, for an [At] or
    [Anytime] date, whether it is "now", the model's horizon, or something
    the model can't answer. [Pricer], [Payoff] and [Replication] all use
    them, so the three can never disagree about dates. *)

open! Core

type t =
  | Unsupported_date of
      { requested : float
      ; model_horizon : float
      }
  (** The contract acquires something at a date other than the density's
      horizon. Pricing it would need a model of how prices move over time. *)
  | Path_dependent of { deadline : float }
  (** The contract contains an [Anytime] whose exercise window is still
      open and ends no later than the horizon. Its value depends on the
      price path, and a single-date density has no path. *)
  | Already_passed of
      { date : float
      ; now : float
      }
  (** An [At] or [Anytime] refers to a date earlier than the moment it is
      acquired, so the opportunity is gone before the holder gets it. *)
  | Off_grid of
      { requested : float
      ; step : float
      }
  (** Only from the lattice pricer: a date that falls between the tree's
      time steps. *)
[@@deriving sexp_of, equal]

(** Two dates closer than 1e-12 years (about 30 microseconds) are the same. *)
val same_time : float -> float -> bool

(** An [At t] acquired at time [now]:
    - [`Now] if t is now (acquiring "at now" is just acquiring);
    - [`Horizon] if t is the model's horizon and still ahead;
    - an error if t has passed or is any other future date. *)
val at_rule : now:float -> horizon:float -> float -> [ `Now | `Horizon | `Error of t ]

(** An [Anytime t] acquired at time [now]: [`Now] if the deadline is now (the
    window has shrunk to this moment); otherwise an error, because either the
    deadline has passed, lies beyond the horizon, or leaves a window whose
    value depends on the price path. *)
val anytime_rule : now:float -> horizon:float -> float -> [ `Now | `Error of t ]
