(* Standard instruments, each defined once, in terms of the combinators.
   Nothing here knows about pricing — that is the whole point. *)

open! Core
open Contract

let spot_minus k = Obs.Sub (Spot, Const k)
let minus_spot k = Obs.Sub (Const k, Spot)

(* $1 at [expiry]. Its price is the discount factor. *)
let zero_coupon ~expiry = at expiry one

(* Obliged to buy at [strike] on [expiry]: payoff S_T - K. *)
let forward ~expiry ~strike = at expiry (scale (spot_minus strike) one)

(* Right, not obligation: at expiry choose the better of (S_T - K) and nothing. *)
let call ~expiry ~strike = at expiry (or_ (scale (spot_minus strike) one) zero)
let put ~expiry ~strike = at expiry (or_ (scale (minus_spot strike) one) zero)

(* Pays $1 if S_T >= strike. *)
let digital_above ~expiry ~strike =
  at expiry (cond (Pred.Ge (Spot, Const strike)) one zero)
;;

(* Pays $1 if S_T < strike. *)
let digital_below ~expiry ~strike =
  at expiry (cond (Pred.Lt (Spot, Const strike)) one zero)
;;

(* Pays $1 if lo <= S_T < hi. *)
let range ~expiry ~lo ~hi =
  at
    expiry
    (cond (Pred.Both (Ge (Spot, Const lo), Lt (Spot, Const hi))) one zero)
;;

(* Long the lo call, short the hi call. *)
let call_spread ~expiry ~lo ~hi =
  and_ (call ~expiry ~strike:lo) (give (call ~expiry ~strike:hi))
;;

(* Long lo and hi calls, short two mid calls. Peaks at mid. *)
let butterfly ~expiry ~lo ~mid ~hi =
  all
    [ call ~expiry ~strike:lo
    ; call ~expiry ~strike:hi
    ; scale_k 2. (give (call ~expiry ~strike:mid))
    ]
;;

(* A digital built from calls: 1 / width of a call spread from
   strike - width/2 to strike + width/2. Its payoff ramps from 0 to 1
   across the spread instead of jumping at the strike, and as the width
   shrinks it becomes the digital. This is how a dealer would hedge a
   Kalshi contract with options, and why a digital's price is the slope of
   the call curve. *)
let digital_by_spread ~expiry ~strike ~width =
  scale_k
    (1. /. width)
    (call_spread ~expiry ~lo:(strike -. (width /. 2.)) ~hi:(strike +. (width /. 2.)))
;;

(* A butterfly of half-width [half_width] around [strike], scaled to pay
   1 / half_width^2 at its peak. Its payoff is a triangle of area 1, so its
   price is a local average of the density at [strike]: Breeden and
   Litzenberger's density as a tradeable contract. *)
let density_butterfly ~expiry ~strike ~half_width =
  scale_k
    (1. /. (half_width *. half_width))
    (butterfly ~expiry ~lo:(strike -. half_width) ~mid:strike ~hi:(strike +. half_width))
;;

(* Path-dependent instruments, built from the same combinators. Only the
   lattice pricer can value them; the others return Path_dependent. *)

(* Exercise at any moment up to expiry, or never: [anytime] makes the timing
   a choice, and [or_ ... zero] makes exercising optional. *)
let american_call ~expiry ~strike = anytime expiry (or_ (scale (spot_minus strike) one) zero)
let american_put ~expiry ~strike = anytime expiry (or_ (scale (minus_spot strike) one) zero)

(* Pays $1 at expiry if the price is ever at or above [barrier] before then.
   Read it as: the holder may lock in "$1 at expiry" at any moment the price
   is at or above the barrier, and must take what's there at expiry
   otherwise. Locking in as soon as the barrier is touched is clearly
   right, so this is exactly a one-touch. *)
let one_touch_above ~expiry ~barrier =
  anytime expiry (cond (Pred.Ge (Spot, Const barrier)) (at expiry one) zero)
;;

(* The same for a price that falls to or below [barrier]. *)
let one_touch_below ~expiry ~barrier =
  anytime expiry (cond (Pred.Ge (Const barrier, Spot)) (at expiry one) zero)
;;
