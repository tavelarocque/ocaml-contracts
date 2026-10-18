(** A volatility smile for one settlement time, as exported by Implied Odds.

    The smile is a raw SVI curve of total implied variance in log-moneyness.
    From it we get a Black implied volatility at every strike, a smooth curve
    of call prices, and from the slope of that curve the risk-neutral
    probability of finishing above any strike (Breeden-Litzenberger).

    Every formula here follows Implied Odds' [data/export/ocaml/README.txt]
    exactly, including the finite-difference step in [prob_above], so that
    this module reproduces its reference probabilities to about 1e-9. *)

open! Core

type params =
  { a : float
  ; b : float
  ; rho : float
  ; m : float
  ; s : float
  }
[@@deriving sexp]

type t [@@deriving sexp_of]

(** [forward] is the forward price to settlement, [years] the time to
    settlement. [k_min]/[k_max] bound the log-moneyness range covered by
    quoted options; outside it the smile is extrapolated. The discount
    factor is 1 (Deribit uses a zero rate). Fails on non-positive forward or
    time, or non-finite parameters. *)
val create
  :  spot:float
  -> forward:float
  -> years:float
  -> k_min:float
  -> k_max:float
  -> params
  -> t Or_error.t

val spot : t -> float
val forward : t -> float
val params : t -> params

(** Where the smile has a near-sharp kink, if it has one: SVI's [s] below
    1e-4 makes the smile almost a V at log-moneyness [m], which packs
    probability into a narrow band of prices there. Returns that price.
    Probabilities near it describe the fit more than the market. *)
val kink : t -> float option
val years : t -> float
val discount : t -> float

(** Is ln(strike / forward) inside the quoted range? *)
val in_quoted_range : t -> float -> bool

(** w(k) = a + b (rho (k - m) + sqrt ((k - m)^2 + s^2)) *)
val total_variance : t -> float -> float

(** Black implied volatility at a strike. *)
val sigma : t -> float -> float

(** Discounted Black-76 call price at a strike. *)
val call : t -> float -> float

(** P(S_T > strike), as minus the slope of the call curve by central
    difference with step h = forward * relative_step (default 1e-4, the
    export's step). For strikes at or below h a one-sided difference is
    used, and at or below 0 the answer is 1. *)
val prob_above : ?relative_step:float -> t -> float -> float

(** E[S_T 1{S_T > strike}] = call / discount + strike * prob_above, from the
    same call curve (and the same step). At strike 0 it is the forward. *)
val partial_mean : ?relative_step:float -> t -> float -> float

(** The implied volatility at the forward. *)
val atm_vol : t -> float

(** The same smile with every implied volatility multiplied by [factor]
    (total variance by factor^2): below 1 a narrower distribution, above 1
    a wider one, with the same forward and the same skew shape. Raises
    unless [factor] is positive. *)
val scale_vol : t -> factor:float -> t

(** No smile at all: one volatility [sigma] at every strike, so the
    distribution is lognormal with the same forward. Raises unless [sigma]
    is positive. *)
val flat : t -> sigma:float -> t

(** The smile after the forward moves to [forward], under one of the two
    textbook rules for how a smile moves with the price:
    - [`Moneyness]: the smile moves with the price, so the vol at a given
      ratio K / F stays the same (SVI's parameters are unchanged);
    - [`Strike]: the vol at each fixed strike stays the same, so the price
      slides along the smile.
    Which one the market follows is an empirical question, and Greeks
    depend on the answer. Spot moves in proportion. Raises unless
    [forward] is positive. *)
val with_forward : t -> forward:float -> sticky:[ `Moneyness | `Strike ] -> t

(** The same smile with [years] to settlement instead: every implied vol at
    a given log-moneyness kept, so total variance scales with time. Used to
    carry an hourly smile forward between snapshots. Raises unless [years]
    is positive. *)
val with_years : t -> years:float -> t

(** The strike K with P(S_T > K) = p, by bisection between forward / 10 and
    forward * 10. With p = 0.5 this is the median. *)
val quantile_above : t -> float -> float
