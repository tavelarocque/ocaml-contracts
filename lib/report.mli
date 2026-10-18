(** The Kalshi-versus-options analysis, as data, so the executable can print
    and save it and the tests can check it.

    A row is "usable" if its strike is inside the smile's quoted range (not
    extrapolated) and its probability doesn't depend on the finite-difference
    step (see [kink_sensitive]). Gaps compare Kalshi's mid price with the
    model, so they need a two-sided quote: a positive bid and an ask. *)

open! Core

module Row : sig
  type t =
    { surface : Export.Surface.t
    ; row : Export.Row.t
    ; q : float (** model P(BTC > strike at close), via the contract language *)
    ; kink_sensitive : bool
    (** shrinking the finite-difference step 100x moves q by over 0.005 *)
    ; usable : bool
    ; mid : float option (** None unless the quote is two-sided *)
    ; half_spread : float option
    ; edge_buy_yes : float option (** per contract, after fee, see [Analysis] *)
    ; edge_sell_yes : float option
    }
end

val analyze : contracts:int -> (Export.Surface.t * Export.Row.t) list -> Row.t list

module Bucket : sig
  type t =
    { kind : Export.Kind.t
    ; lo : float
    ; hi : float
    ; n : int
    ; mean_gap : float (** Kalshi mid minus model probability *)
    ; median_gap : float
    ; mean_half_spread : float
    }
end

(** Usable two-sided rows, grouped by kind and model probability. *)
val buckets : Row.t list -> Bucket.t list

module Actionable : sig
  type t =
    { kind : Export.Kind.t
    ; side : [ `Buy_yes | `Sell_yes ]
    ; rows : int (** quotes with positive edge, counting every snapshot *)
    ; contracts : int (** distinct (event, strike) among them *)
    ; events : int
    ; mean_edge : float
    }
end

(** Usable rows where trading at the touch beats the fee under the model,
    leaving out quotes the model puts below 5% or above 95% (there Kalshi's
    1-cent tick and the requirement of a bid decide which quotes survive)
    and quotes showing fewer than [contracts] at the touch. *)
val actionable : contracts:int -> Row.t list -> Actionable.t list

module Shape : sig
  type t =
    { kind : Export.Kind.t
    ; snapshots : int (** (snapshot, event) pairs measured *)
    ; events : int (** distinct events among them *)
    ; median_iqr_ratio : float (** Kalshi's interquartile range / the model's *)
    ; iqr_ratio_p25 : float
    ; iqr_ratio_p75 : float
    ; share_wider : float (** fraction of snapshots with ratio > 1 *)
    ; median_shift : float (** Kalshi median minus model median, dollars *)
    ; median_method_bias : float
    (** median of (model width by the ladder method) / (smile's exact width):
        how much the method alone stretches a distribution, before the
        same-method comparison cancels it *)
    }
end

(** Per (snapshot, event): the shape of Kalshi's implied distribution next
    to the model's, from usable two-sided mid prices. *)
val shapes : Row.t list -> (Export.Surface.t * Analysis.Event_shape.t) list

val shape_summary : (Export.Surface.t * Analysis.Event_shape.t) list -> Shape.t list

(** Median and linear-interpolated quantiles of a non-empty list. *)
val quantile : float list -> float -> float
