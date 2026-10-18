(** Markouts: was the trader on the other side better informed?

    A trade in a Kalshi contract has a taker, who crossed the spread, and a
    passive side, whose resting quote was hit. The passive side's markout
    at horizon h is what that trade was worth to it, marked against fair
    value h later:
      taker bought YES at P:  P - V(t + h)
      taker sold YES at P:    V(t + h) - P
    At h = 0 it is the half-spread the taker paid. If takers know nothing,
    V is a martingale and the markout stays at that level at every
    horizon. If takers see where the price is going, it falls with h, and
    turns negative once their information is worth more than the spread.
    That is the number that decides whether quoting can pay.

    Fair value V comes from the options: the latest hourly smile for the
    event, moved by the BTC index's change since that snapshot (sticky
    moneyness; the same index series at both ends)
    and shortened to the time left (same vols), then priced through the
    contract language. At the close it is the payout implied by the index:
    1 if above the strike. *)

open! Core

module Fair : sig
  type t

  (** [max_smile_age] (default 75 minutes) and [max_index_age] (default
      30 seconds; 90 suits 1-minute prices) bound how stale an input may
      be; beyond them there is no fair value. *)
  val create
    :  ?max_smile_age:float
    -> ?max_index_age:float
    -> surfaces:Export.Surface.t list
    -> index:(float * float) array
    -> unit
    -> t Or_error.t

  val index_at : t -> float -> float option

  (** P(BTC above [strike] at [close]), as seen at [at]. At or after the
      close, the payout from the index. *)
  val digital : t -> event:string -> strike:float -> close:float -> at:float -> float option
end

type t =
  { trade : Hf.Trade.t
  ; event : string
  ; strike : float
  ; close : float
  ; after : float option array (** passive markout at each horizon *)
  ; at_close : float option (** against the payout at the close *)
  }
[@@deriving sexp_of]

(** Markouts of every trade whose market appears in [books] (which give
    each ticker's event, strike and close). *)
val compute
  :  Fair.t
  -> books:Hf.Book.t list
  -> trades:Hf.Trade.t list
  -> horizons:float list
  -> t list

module Summary : sig
  type row =
    { label : string
    ; trades : int
    ; contracts : float
    ; mean : float (** volume-weighted mean passive markout, dollars per contract *)
    ; se : float (** standard error, clustered by event *)
    ; ci_low : float (** 95% interval from resampling whole events *)
    ; ci_high : float
    }
  [@@deriving sexp_of]

  (** One row per horizon, then one for the close. Trades without a fair
      value at a horizon are left out of that row. *)
  val compute : ?resamples:int -> ?seed:int -> horizons:float list -> t list -> row list
end
