(** A risk-neutral distribution of the underlying price at one horizon,
    stored as a discrete grid of (price, probability) points.

    The type is abstract: other modules can't build a [t] by hand, so every
    [t] in the program has passed the checks in [create] (probabilities are
    non-negative and sum to 1). That guarantee is the point of the [.mli]. *)

open! Core

type t [@@deriving sexp_of]

(** Build a density from (price, probability) points. Fails on an empty
    list, non-finite values, negative probabilities, or probabilities whose
    sum is more than [tolerance] (default 1e-6) away from 1. A sum within
    tolerance is rescaled to exactly 1. Real exported grids lose a little
    mass in the tails, so pass a looser tolerance (say 1e-3) for those. *)
val create
  :  ?tolerance:float
  -> spot0:float
  -> horizon:float
  -> rate:float
  -> (float * float) list
  -> t Or_error.t

(** Parse lines of [price,probability] into a density. Blank lines, lines
    starting with [#], and a [price,probability] header are skipped. Numbers
    must be plain decimals or scientific notation. Every malformed line is
    reported, each with its line number. The result goes through [create],
    with the same [tolerance]. *)
val of_csv_lines
  :  ?tolerance:float
  -> spot0:float
  -> horizon:float
  -> rate:float
  -> string list
  -> t Or_error.t

(** [of_csv_lines] on the lines of a file. *)
val of_csv
  :  ?tolerance:float
  -> spot0:float
  -> horizon:float
  -> rate:float
  -> string
  -> t Or_error.t

(** The Black-Scholes world: log-normal terminal price with volatility [vol].
    A toy model for testing the pricer before real densities arrive. *)
val lognormal
  :  ?n:int
  -> ?width_sd:float
  -> spot0:float
  -> horizon:float
  -> rate:float
  -> vol:float
  -> unit
  -> t

val spot0 : t -> float
val horizon : t -> float
val rate : t -> float
(** A fresh copy of the points, sorted by price. *)
val points : t -> (float * float) array

(** Value today of $1 paid at the horizon: exp(-rate * horizon). *)
val discount : t -> float

(** Expected value of [f(S_T)] under this distribution (not discounted). *)
val expectation : t -> f:(float -> float) -> float
