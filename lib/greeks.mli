(** How a contract's price responds to the market, by bumping the smile and
    pricing again.

    A market maker holding Kalshi contracts is exposed to three things:
    BTC's level moving (delta, and gamma for how delta itself moves) and
    the options market's vols moving (vega). None of these needs new
    pricing code. Each is the same contract priced under a slightly
    different smile, so they work for any contract the replication pricer
    can value, under any [Model_choice].

    Bumps move the forward, which for a contract settled on the price at
    the close is the right underlying: Deribit's futures. *)

open! Core

type t =
  { value : float
  ; delta : float (** dV/dF: per $1 move in the forward *)
  ; gamma : float (** d2V/dF2 *)
  ; vega : float (** change in V when every vol rises by 1% of itself *)
  }
[@@deriving sexp_of]

(** Central differences with forward bumps of [relative_bump] * F and vol
    bumps of +-1%. The default bump is a fiftieth of the distribution's
    standard deviation (F * ATM vol * sqrt T / 50): about $50 for a day at
    BTC's level, about $3 for the last hour. A fixed dollar bump would be
    too coarse for hourly contracts and needlessly fine for daily ones. [sticky] says
    how the smile moves with the forward (see [Smile.with_forward]). *)
val compute
  :  ?relative_bump:float
  -> sticky:[ `Moneyness | `Strike ]
  -> Model_choice.t
  -> Smile.t
  -> Contract.t
  -> (t, Payoff.Error.t) Result.t

val compute_exn
  :  ?relative_bump:float
  -> sticky:[ `Moneyness | `Strike ]
  -> Model_choice.t
  -> Smile.t
  -> Contract.t
  -> t
