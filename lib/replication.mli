(** A second pricer: static replication from a smooth call curve.

    The grid pricer ([Pricer]) values a contract by evaluating its payoff at
    every grid point. That is general, but a digital's price is then off by
    up to one grid bin (see the digital tests). This pricer instead turns
    the payoff at the horizon into a piecewise-linear function ([Payoff])
    and prices each piece exactly from two functions of the strike:

    - P(K) = P(S_T >= K), the digital probability, and
    - M(K) = E[S_T 1{S_T >= K}], the partial mean, which comes from call
      prices: M(K) = C(K) / DF + K P(K).

    (For a continuous distribution, ">=" and ">" are the same thing; the
    distinction only matters for a grid with a point exactly on a strike.)

    A piece paying a + b S on [lo, hi) is worth
      DF * (a (P(lo) - P(hi)) + b (M(lo) - M(hi))).
    That is Carr & Madan's static replication: any piecewise-linear payoff
    is a portfolio of bonds, digitals and calls, so it needs nothing beyond
    those two curves. *)

open! Core

module Model : sig
  type t

  (** [prob_above] and [partial_mean] are only called at strikes strictly
      between 0 and infinity; the ends are handled here. [mean] is E[S_T]. *)
  val create
    :  spot0:float
    -> horizon:float
    -> discount:float
    -> mean:float
    -> prob_above:(float -> float)
    -> partial_mean:(float -> float)
    -> t

  (** The same distribution the grid pricer uses. Prices agree with
      [Pricer] to rounding, including when a grid point lies exactly on a
      strike. That agreement is the check that both pricers are right. The
      one systematic difference: replication reports an error from any
      branch some price could reach, the grid pricer only from branches its
      grid points reach. When several parts of a contract are unpriceable,
      the two may also report different ones. *)
  val of_density : Density.t -> t

  (** An Implied Odds smile. Digital probabilities are the slope of its call
      curve by central difference, exactly as Implied Odds computes them;
      calls and spreads, whose payoffs are continuous, come out matching
      the smile's own call prices. [relative_step] is the finite
      difference's step as a fraction of the forward (default 1e-4, the
      export's); a distribution only minutes from settlement needs a
      smaller one. *)
  val of_smile : ?relative_step:float -> Smile.t -> t

  (** A lognormal distribution with mean [forward] and volatility [sigma]
      over [horizon] years, in closed form: P(S_T >= K) = N(d2) and
      M(K) = F N(d1). No finite differences, so it is an independent check
      on [of_smile] applied to a flat smile. Raises unless forward, sigma
      and horizon are positive. *)
  val lognormal
    :  spot0:float
    -> horizon:float
    -> discount:float
    -> forward:float
    -> sigma:float
    -> t

  val spot0 : t -> float
  val horizon : t -> float
  val discount : t -> float
  val mean : t -> float

  (** P(S_T >= k), with P = 1 at or below 0 and 0 at infinity. *)
  val prob_above : t -> float -> float

  (** E[S_T 1{S_T >= k}], with the mean at or below 0 and 0 at infinity. *)
  val partial_mean : t -> float -> float
end

val price : Model.t -> Contract.t -> (float, Payoff.Error.t) Result.t
val price_exn : Model.t -> Contract.t -> float
