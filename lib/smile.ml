open! Core

type params =
  { a : float
  ; b : float
  ; rho : float
  ; m : float
  ; s : float
  }
[@@deriving sexp]

type t =
  { spot : float
  ; forward : float
  ; years : float
  ; k_min : float
  ; k_max : float
  ; params : params
  }
[@@deriving sexp_of]

let create ~spot ~forward ~years ~k_min ~k_max params =
  let { a; b; rho; m; s } = params in
  if not (List.for_all [ spot; forward; years; k_min; k_max; a; b; rho; m; s ] ~f:Float.is_finite)
  then Or_error.error_s [%message "non-finite value in smile"]
  else if Float.(forward <= 0. || years <= 0.)
  then Or_error.error_s [%message "forward and time must be positive" (forward : float) (years : float)]
  else Ok { spot; forward; years; k_min; k_max; params }
;;

let spot t = t.spot
let forward t = t.forward
let params t = t.params
let kink t = Option.some_if Float.(t.params.s < 1e-4) (t.forward *. Float.exp t.params.m)
let years t = t.years
let discount _ = 1.

let in_quoted_range t strike =
  let k = Float.log (strike /. t.forward) in
  Float.(t.k_min <= k && k <= t.k_max)
;;

let total_variance t k =
  let { a; b; rho; m; s } = t.params in
  let x = k -. m in
  a +. (b *. ((rho *. x) +. Float.sqrt ((x *. x) +. (s *. s))))
;;

let sigma t strike =
  let w = total_variance t (Float.log (strike /. t.forward)) in
  Float.sqrt (Float.max w 1e-10 /. t.years)
;;

(* Standard normal CDF. Written with erfc rather than (1 + erf) / 2 so that
   far in the lower tail, where N(x) is tiny, it doesn't lose every digit to
   cancellation. Near the money the two differ by a few units in the last
   place, far below the 1e-9 this module must match. *)
let normal_cdf x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.)

let call t strike =
  if Float.(strike <= 0.)
  then (* A call struck at or below zero always pays S - K. *)
    discount t *. (t.forward -. strike)
  else (
    let v = sigma t strike *. Float.sqrt t.years in
    let d1 = (Float.log (t.forward /. strike) +. (v *. v /. 2.)) /. v in
    let d2 = d1 -. v in
    discount t *. ((t.forward *. normal_cdf d1) -. (strike *. normal_cdf d2)))
;;

let prob_above ?(relative_step = 1e-4) t strike =
  let h = t.forward *. relative_step in
  if Float.(strike <= 0.)
  then 1.
  else if Float.(strike <= h)
  then
    (* Too close to zero to step below: a one-sided difference instead.
       Only strikes under about $8 get here, far from any Kalshi contract. *)
    -.(call t (strike +. h) -. call t strike) /. h /. discount t
  else -.(call t (strike +. h) -. call t (strike -. h)) /. (2. *. h) /. discount t
;;

let partial_mean ?relative_step t strike =
  if Float.(strike <= 0.)
  then t.forward
  else (call t strike /. discount t) +. (strike *. prob_above ?relative_step t strike)
;;

let atm_vol t = sigma t t.forward

let scale_vol t ~factor =
  if not Float.(is_finite factor && factor > 0.)
  then raise_s [%message "Smile.scale_vol: factor must be positive" (factor : float)];
  (* Every vol times [factor] is every total variance times factor^2, and
     total variance is linear in a and b. *)
  let f2 = factor *. factor in
  { t with params = { t.params with a = t.params.a *. f2; b = t.params.b *. f2 } }
;;

let flat t ~sigma =
  if not Float.(is_finite sigma && sigma > 0.)
  then raise_s [%message "Smile.flat: sigma must be positive" (sigma : float)];
  (* b = 0 removes the smile: w(k) = a everywhere. *)
  { t with params = { a = sigma *. sigma *. t.years; b = 0.; rho = 0.; m = 0.; s = 0. } }
;;

let with_forward t ~forward ~sticky =
  if not Float.(is_finite forward && forward > 0.)
  then raise_s [%message "Smile.with_forward: forward must be positive" (forward : float)];
  let d = Float.log (forward /. t.forward) in
  let spot = t.spot *. forward /. t.forward in
  match sticky with
  | `Moneyness -> { t with forward; spot }
  | `Strike ->
    (* Keep the vol at every strike. A strike K sits at log-moneyness
       k' = ln(K / F') = k - d after the move, and SVI depends on k - m,
       so moving m (and the quoted range) by -d leaves w at K unchanged. *)
    { t with
      forward
    ; spot
    ; k_min = t.k_min -. d
    ; k_max = t.k_max -. d
    ; params = { t.params with m = t.params.m -. d }
    }
;;

let with_years t ~years =
  if not Float.(is_finite years && years > 0.)
  then raise_s [%message "Smile.with_years: years must be positive" (years : float)];
  (* Same vol at every log-moneyness, less time: total variance w = vol^2 T
     scales with T, and w is linear in a and b. *)
  let r = years /. t.years in
  { t with years; params = { t.params with a = t.params.a *. r; b = t.params.b *. r } }
;;

let quantile_above t p =
  (* Bisection in log-strike: P(S > K) falls as K rises, so keep the half
     where it crosses p. 200 halvings of a factor-of-100 range pin the
     strike to far below a cent. *)
  let rec go lo hi n =
    if n = 0
    then Float.exp ((lo +. hi) /. 2.)
    else (
      let mid = (lo +. hi) /. 2. in
      if Float.(prob_above t (Float.exp mid) > p) then go mid hi (n - 1) else go lo mid (n - 1))
  in
  let f = Float.log t.forward in
  go (f -. Float.log 10.) (f +. Float.log 10.) 200
;;
