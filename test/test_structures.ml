(* Ranges, ladders and butterflies from one density, checked against a
   lognormal whose density and digitals are known in closed form.
   Synthetic smile only. *)

open! Core
open Contracts

let smile = Synthetic.smile ()
let expiry = Smile.years smile
let forward = Smile.forward smile
let sigma = Smile.atm_vol smile
let model = Model_choice.model Lognormal_atm smile
let price = Replication.price_exn model
let v = sigma *. Float.sqrt expiry
let normal_pdf x = Float.exp (-.(x *. x) /. 2.) /. Float.sqrt (2. *. Float.pi)

(* The lognormal's density at k: phi(d2) / (k v). *)
let density k =
  let d2 = (Float.log (forward /. k) -. (v *. v /. 2.)) /. v in
  normal_pdf d2 /. (k *. v)
;;

let%expect_test "a call spread becomes the digital as it narrows" =
  (* The spread's price is the average of P(S_T >= K) across the spread, so
     it misses the digital by about width^2 / 24 times P's curvature: 10x
     narrower, about 100x closer.

     The $20 column is worth a second look: Implied Odds' p_opt is a
     central difference of call prices with step F * 1e-4 = $10 either
     side, which is exactly this $20-wide call spread. Its 5e-7 here is
     the same 5e-7 test_models finds between the export's method and the
     exact digital. *)
  List.iter [ 98_000.; 100_000.; 103_000. ] ~f:(fun strike ->
    let digital = price (Kalshi.above ~expiry ~strike) in
    printf "strike %.0f, digital %.6f, spread minus digital:" strike digital;
    List.iter [ 2000.; 200.; 20. ] ~f:(fun width ->
      printf "  w %4.0f %+.1e" width
        (price (Instruments.digital_by_spread ~expiry ~strike ~width) -. digital));
    printf "\n");
  [%expect {|
    strike 98000, digital 0.773482, spread minus digital:  w 2000 -5.3e-03  w  200 -5.4e-05  w   20 -5.4e-07
    strike 100000, digital 0.494723, spread minus digital:  w 2000 +3.6e-04  w  200 +3.8e-06  w   20 +3.8e-08
    strike 103000, digital 0.129144, spread minus digital:  w 2000 +5.4e-03  w  200 +5.5e-05  w   20 +5.5e-07
    |}]
;;

let%expect_test "butterflies and narrow ranges both measure the density" =
  (* Two contracts whose prices estimate the density at a strike: a range
     [K - h/2, K + h/2) divided by h, and a butterfly scaled to unit area.
     Against the closed-form density, both converge as h shrinks. *)
  printf "%8s %12s %12s %12s\n" "strike" "density" "range/h" "butterfly";
  List.iter [ 96_000.; 100_000.; 104_000. ] ~f:(fun strike ->
    let h = 100. in
    printf "%8.0f %12.4e %12.4e %12.4e\n" strike (density strike)
      (price (Instruments.range ~expiry ~lo:(strike -. (h /. 2.)) ~hi:(strike +. (h /. 2.))) /. h)
      (price (Instruments.density_butterfly ~expiry ~strike ~half_width:h)));
  [%expect {|
    strike      density      range/h    butterfly
     96000   4.8750e-05   4.8753e-05   4.8757e-05
    100000   1.5077e-04   1.5076e-04   1.5075e-04
    104000   4.7379e-05   4.7383e-05   4.7386e-05
    |}]
;;

let%expect_test "a range ladder is the digitals' differences" =
  (* Each range [lo, hi) costs P(lo) - P(hi), so the ladder's prices are
     the steps down a digital curve, and the whole ladder sums to $1. *)
  let edges = Kalshi.edges_around ~centre:forward ~step:1000. ~each_side:2 in
  let total = ref 0. in
  List.iter (Kalshi.ladder ~expiry ~edges) ~f:(fun (label, c) ->
    let p = price c in
    total := !total +. p;
    printf "%-20s %.4f\n" label p);
  printf "%-20s %.12f\n" "total" !total;
  [%expect {|
    below 98000          0.2265
    98000 to 99000       0.1304
    99000 to 100000      0.1483
    100000 to 101000     0.1462
    101000 to 102000     0.1254
    102000 or above      0.2231
    total                1.000000000000
    |}]
;;
