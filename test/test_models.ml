(* Alternative distributions behind the one model interface. Synthetic
   smiles only. *)

open! Core
open Contracts

let strikes f = List.map [ 0.9; 0.95; 0.98; 0.99; 1.; 1.01; 1.02; 1.05; 1.1 ] ~f:(( *. ) f)

let%expect_test "closed-form lognormal = a flat smile through finite differences" =
  (* Two independent routes to the same distribution: N(d2) written out,
     against minus the slope of Black-76 call prices from a smile with
     b = 0. They share nothing but the normal CDF.

     They differ by about 5e-7, and that is the export's finite difference,
     not a bug: a central difference with step h is off by h^2 / 6 times
     the slope of the density, and with h = F * 1e-4 = $10 on a one-day
     distribution about $2,600 wide that comes to a few 1e-7. A step 10x
     smaller cuts it about 100x, as h^2 says. Implied Odds' p_opt carries
     the same error, which is why matching it to 1e-9 means using its step,
     and why 1e-6 is the honest accuracy of any one probability. *)
  let smile = Synthetic.smile () in
  let sigma = Smile.atm_vol smile in
  let flat = Smile.flat smile ~sigma in
  let closed = Model_choice.model Lognormal_atm smile in
  let worst f =
    List.fold (strikes (Smile.forward smile)) ~init:0. ~f:(fun acc k ->
      Float.max acc (Float.abs (f k -. Replication.Model.prob_above closed k)))
  in
  printf "at-the-money vol %.4f\n" sigma;
  List.iter [ 1e-4; 1e-5 ] ~f:(fun relative_step ->
    printf "step F x %.0e: max |P difference| %.1e\n" relative_step
      (worst (Smile.prob_above ~relative_step flat)));
  [%expect {|
    at-the-money vol 0.5055
    step F x 1e-04: max |P difference| 5.4e-07
    step F x 1e-05: max |P difference| 5.4e-09
    |}]
;;

let%expect_test "every model is a distribution with the smile's forward" =
  (* The forward struck at F is worth nothing, $1 at expiry is worth the
     discount factor, and a ladder covering every price sums to $1. These
     hold whatever the shape, so they hold for each choice. *)
  let smile = Synthetic.smile () in
  let f = Smile.forward smile
  and expiry = Smile.years smile in
  let edges = strikes f in
  List.iter (Model_choice.defaults @ [ Scaled_smile 1.3 ]) ~f:(fun choice ->
    let model = Model_choice.model choice smile in
    let price = Replication.price_exn model in
    let ladder =
      List.sum (module Float) (Kalshi.ladder ~expiry ~edges) ~f:(fun (_, c) -> price c)
    in
    printf "%-11s forward %+.1e  bond %.12f  ladder %.12f\n" (Model_choice.name choice)
      (price (Instruments.forward ~expiry ~strike:f) /. f)
      (price (Instruments.zero_coupon ~expiry))
      ladder);
  [%expect {|
    smile       forward +0.0e+00  bond 1.000000000000  ladder 1.000000000000
    lognormal   forward +0.0e+00  bond 1.000000000000  ladder 1.000000000000
    smile x0.80 forward +0.0e+00  bond 1.000000000000  ladder 1.000000000000
    smile x1.30 forward +0.0e+00  bond 1.000000000000  ladder 1.000000000000
    |}]
;;

let%expect_test "scaling the vols scales the width" =
  (* For a flat smile, log S_T is normal with sd sigma sqrt(T), so the
     interquartile range of log S_T scales exactly with sigma. With a skew
     it is only close: scaling w by f^2 also scales the skew's effect. *)
  let width smile =
    Float.log (Smile.quantile_above smile 0.25) -. Float.log (Smile.quantile_above smile 0.75)
  in
  let skewed = Synthetic.smile () in
  let flat = Smile.flat skewed ~sigma:(Smile.atm_vol skewed) in
  List.iter [ 0.5; 0.8; 1.3 ] ~f:(fun factor ->
    printf "factor %.1f: flat %.6f  skewed %.6f\n" factor
      (width (Smile.scale_vol flat ~factor) /. width flat)
      (width (Smile.scale_vol skewed ~factor) /. width skewed));
  [%expect {|
    factor 0.5: flat 0.500004  skewed 0.510785
    factor 0.8: flat 0.800001  skewed 0.808057
    factor 1.3: flat 1.299999  skewed 1.276483
    |}]
;;

let%expect_test "the same digitals under each model" =
  (* Where the models disagree is the information in the smile's shape.
     The skew puts more probability far below the forward (above 95,000:
     0.966 under the smile, 0.973 lognormal), and the smile's upward curve
     puts a little more far above it (above 110,000). Both have the same
     forward, so the middle shifts to compensate: "above 100,000" is 0.504
     under the smile and 0.495 lognormal. Narrowing to 80% pulls every
     digital toward 0 or 1. *)
  let smile = Synthetic.smile () in
  let expiry = Smile.years smile in
  printf "%8s" "strike";
  List.iter Model_choice.defaults ~f:(fun c -> printf " %11s" (Model_choice.name c));
  printf "\n";
  List.iter (strikes (Smile.forward smile)) ~f:(fun strike ->
    printf "%8.0f" strike;
    List.iter Model_choice.defaults ~f:(fun choice ->
      printf " %11.4f"
        (Replication.price_exn (Model_choice.model choice smile)
           (Kalshi.above ~expiry ~strike)));
    printf "\n");
  [%expect {|
    strike       smile   lognormal smile x0.80
     90000      0.9994      1.0000      1.0000
     95000      0.9657      0.9729      0.9878
     98000      0.7822      0.7735      0.8309
     99000      0.6553      0.6431      0.6875
    100000      0.5038      0.4947      0.5030
    101000      0.3507      0.3485      0.3167
    102000      0.2202      0.2231      0.1695
    105000      0.0314      0.0316      0.0105
    110000      0.0003      0.0001      0.0000
    |}]
;;
