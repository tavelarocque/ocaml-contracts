(** Comparing Kalshi's quotes with the options-implied probabilities.

    Everything here is a comparison of two markets' prices. The model's
    probabilities are risk-neutral: they include the premium option buyers
    pay for protection, so they are not forecasts, and a gap between the
    two is not by itself an edge. *)

open! Core

(** Kalshi's taker fee per contract, in dollars, for an order of
    [contracts] at [price]: 0.07 * price * (1 - price) per contract, with
    the order's total rounded up to the next cent. Matches Implied Odds'
    [implied/kalshi_ladder.py]. *)
val taker_fee : price:float -> contracts:int -> float

(** Expected profit per contract, under the model, of trading at the touch
    and paying the taker fee: buying YES at the ask is worth
    q - ask - fee(ask); selling YES at the bid (buying NO) is worth
    bid - q - fee(bid). [None] when that side has no quote. *)
val edge_buy_yes : q:float -> ask:float option -> contracts:int -> float option
val edge_sell_yes : q:float -> bid:float option -> contracts:int -> float option

(** The strike at which an "above K" ladder's price falls to [p], from
    (strike, price) points. Prices are first made non-increasing in strike
    (a later strike can't be likelier than an earlier one). Between two
    strikes, interpolation is linear in the normal quantile of the price
    rather than in the price itself: a probability curve is S-shaped, and a
    straight line between strikes would push both quartiles outward,
    making any distribution look wider than it is. [None] if the ladder
    never crosses [p]. *)
val ladder_quantile : (float * float) list -> float -> float option

(** The inverse of the standard normal CDF, for p strictly between 0 and 1. *)
val probit : float -> float

module Event_shape : sig
  type t =
    { kalshi_iqr : float (** strike at P = 0.25 minus strike at P = 0.75 *)
    ; model_iqr : float (** the same, from the model's probabilities at the same strikes *)
    ; model_iqr_exact : float (** the model's own, from the smile, for scale *)
    ; kalshi_median : float
    ; model_median : float (** at the same strikes, by the same method *)
    }
  [@@deriving sexp_of]

  (** How wide and where the distribution Kalshi's mid prices imply sits,
      next to the model's. Both are measured the same way, from
      (strike, Kalshi mid, model probability) at the same strikes, so
      whatever the strike spacing and interpolation do to one, they do to
      the other. [model_iqr_exact] shows how far that method is from the
      smile's true width. [None] if either ladder doesn't span the
      quartiles. *)
  val compute : Smile.t -> (float * float * float) list -> t option
end
