(** Payoffs as piecewise-linear functions of the price at the horizon.

    Almost every contract in this project pays, at the horizon, an amount
    that is linear in the price between a few breakpoints: a digital is 0
    then 1, a call is 0 then S - K, a ladder bucket is 0, 1, 0. Writing the
    payoff in that shape is a fourth interpreter of the contract language,
    and it is what lets [Replication] price a contract exactly from smooth
    call prices instead of summing over a grid.

    Every piece includes its left end and excludes its right end, written
    [lo, hi). Predicates are cut so that this is exactly right at the
    threshold ("S >= c" starts a piece at c; "S <= c" ends one just above
    c), so a density with an atom exactly on a strike is priced the same way
    the grid pricer prices it. *)

open! Core

(** On [lo, hi) the payoff is [a + b * S]. Pieces are sorted, contiguous,
    start at lo = 0 and end at hi = infinity. *)
type piece =
  { lo : float
  ; hi : float
  ; a : float
  ; b : float
  }
[@@deriving sexp_of]

type t = piece list [@@deriving sexp_of]

module Error : sig
  type t =
    | Unpriceable of Unpriceable.t
    (** The same date problems the grid pricer reports. *)
    | Nonlinear of Contract.Obs.t
    (** An observable multiplies two price-dependent amounts, e.g. [Spot *
        Spot], or scales a price-dependent payoff by one. The result is a
        curve, not a piecewise line. This is reported even where the curve
        would end up multiplied by zero: the check is on the contract's
        shape, not its value. *)
  [@@deriving sexp_of, equal]
end

(** The payoff of acquiring the contract at [horizon], as a function of the
    price then. *)
val of_contract : horizon:float -> Contract.t -> (t, Error.t) Result.t

(** The payoff at a price. For tests and inspection. *)
val eval : t -> float -> float
