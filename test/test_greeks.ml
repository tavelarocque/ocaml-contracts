(* Greeks by bump and reprice, checked against Black's closed forms and
   against laws any model must satisfy. Synthetic smiles only. *)

open! Core
open Contracts

let normal_pdf x = Float.exp (-.(x *. x) /. 2.) /. Float.sqrt (2. *. Float.pi)
let normal_cdf x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.)

let%expect_test "bump-and-reprice Greeks match Black's formulas" =
  (* On a flat smile the distribution is lognormal, and Black gives every
     Greek in closed form. With d1, d2 as usual and v = sigma sqrt(T):
       digital: delta = phi(d2) / (F v),  vega per 1% = -phi(d2) d1 / 100
       call:    delta = N(d1),  gamma = phi(d1) / (F v),
                vega per 1% = F phi(d1) v / 100.
     The bumps are a fiftieth of the distribution's width in the forward
     and 1% in the vol, so the printed relative errors are the central
     differences' own error, of order (1/50)^2 / 6, about 7e-5. *)
  let base = Synthetic.smile () in
  let sigma = Smile.atm_vol base in
  let smile = Smile.flat base ~sigma in
  let f = Smile.forward smile
  and expiry = Smile.years smile in
  let v = sigma *. Float.sqrt expiry in
  let rel got want = (got -. want) /. Float.abs want in
  printf "%8s %-8s %10s %10s %10s\n" "strike" "" "delta" "gamma" "vega";
  List.iter [ 97_000.; 100_000.; 103_000. ] ~f:(fun k ->
    let d1 = (Float.log (f /. k) +. (v *. v /. 2.)) /. v in
    let d2 = d1 -. v in
    let g c = Greeks.compute_exn ~sticky:`Strike Lognormal_atm smile c in
    let dg = g (Kalshi.above ~expiry ~strike:k)
    and cg = g (Instruments.call ~expiry ~strike:k) in
    printf "%8.0f %-8s %+10.1e %10s %+10.1e\n" k "digital"
      (rel dg.delta (normal_pdf d2 /. (f *. v))) "" (rel dg.vega (-.normal_pdf d2 *. d1 /. 100.));
    printf "%8s %-8s %+10.1e %+10.1e %+10.1e\n" "" "call" (rel cg.delta (normal_cdf d1))
      (rel cg.gamma (normal_pdf d1 /. (f *. v)))
      (rel cg.vega (f *. normal_pdf d1 *. v /. 100.)));
  [%expect {|
    strike               delta      gamma       vega
     97000 digital    +2.6e-05              +2.5e-05
           call       -1.8e-05   +1.5e-05   -3.7e-05
    100000 digital    -6.7e-05              +2.9e-09
           call       -2.1e-06   -3.3e-05   -2.9e-09
    103000 digital    +1.3e-05              -1.9e-05
           call       +1.2e-04   +4.4e-06   -3.6e-05
    |}]
;;

let%expect_test "laws: the forward, put-call parity, and a full ladder" =
  (* Whatever the model and whichever way the smile moves: a forward
     contract has delta 1 and no gamma or vega; a call minus a put is that
     forward, so their deltas differ by exactly 1; and a ladder covering
     every price always pays $1, so its Greeks sum to zero. Checked is the
     largest violation across strikes, models and both sticky rules. *)
  let smile = Synthetic.smile () in
  let f = Smile.forward smile
  and expiry = Smile.years smile in
  let worst = ref 0. in
  let note x = worst := Float.max !worst (Float.abs x) in
  List.iter [ `Moneyness; `Strike ] ~f:(fun sticky ->
    List.iter Model_choice.defaults ~f:(fun choice ->
      let g = Greeks.compute_exn ~sticky choice smile in
      let fwd = g (Instruments.forward ~expiry ~strike:f) in
      note (fwd.delta -. 1.);
      note (fwd.gamma *. f);
      note (fwd.vega /. f);
      List.iter [ 96_000.; 100_000.; 104_000. ] ~f:(fun strike ->
        let c = g (Instruments.call ~expiry ~strike)
        and p = g (Instruments.put ~expiry ~strike) in
        note (c.delta -. p.delta -. 1.));
      let ladder =
        Kalshi.ladder ~expiry ~edges:(Kalshi.edges_around ~centre:f ~step:1000. ~each_side:3)
        |> List.map ~f:(fun (_, c) -> g c)
      in
      note (List.sum (module Float) ladder ~f:(fun x -> x.delta) *. f);
      note (List.sum (module Float) ladder ~f:(fun x -> x.vega))));
  (* About 1e-13: rounding error, whose last digit differs between platforms, so a bound. *)
  printf "largest violation below 1e-11: %b\n" Float.(!worst < 1e-11);
  [%expect {| largest violation below 1e-11: true |}]
;;

let%expect_test "a digital's delta is the density, if the smile moves with the price" =
  (* If the smile moves with the price, P(S_T >= K) depends on K and F only
     through K / F, and differentiating that gives
       dP/dF = (K / F) * density(K):
     a Kalshi contract's delta is the height of the distribution at its
     strike. The density here comes from a butterfly, a different contract
     priced the other way, so this is a real check.

     If instead vols stay put at each strike, moving the price also slides
     the strike along the skew, and the delta picks up a vega-times-skew
     term. With this skew (puts dearer than calls, rho < 0) the delta is
     9% higher at 96,000 and 4% lower at the money. A steeper skew moves it
     more. A hedge sized on the wrong rule is off by that much, so how the
     smile moves is not a detail. *)
  let smile = Synthetic.smile () in
  let f = Smile.forward smile
  and expiry = Smile.years smile in
  let price = Replication.price_exn (Model_choice.model Smile smile) in
  printf "%8s %16s %16s %16s\n" "strike" "(K/F) density" "delta, moneyness" "delta, strike";
  List.iter [ 96_000.; 98_000.; 100_000.; 102_000.; 104_000. ] ~f:(fun strike ->
    let density = price (Instruments.density_butterfly ~expiry ~strike ~half_width:50.) in
    let delta sticky =
      (Greeks.compute_exn ~sticky Smile smile (Kalshi.above ~expiry ~strike)).delta
    in
    printf "%8.0f %16.4e %16.4e %16.4e\n" strike (strike /. f *. density) (delta `Moneyness)
      (delta `Strike));
  [%expect {|
    strike    (K/F) density delta, moneyness    delta, strike
     96000       4.2600e-05       4.2603e-05       4.6281e-05
     98000       1.0761e-04       1.0761e-04       1.0836e-04
    100000       1.5672e-04       1.5671e-04       1.5081e-04
    102000       1.1587e-04       1.1587e-04       1.1267e-04
    104000       4.7084e-05       4.7087e-05       4.7815e-05
    |}]
;;
