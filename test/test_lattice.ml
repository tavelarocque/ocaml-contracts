(* The implied tree. Most tests here check results with a known answer:
   identities that must hold on any arbitrage-free tree, classic theorems,
   and a closed-form formula. *)

open! Core
open Contracts
module G = Quickcheck.Generator

let expiry = 7. /. 365.
let spot0 = 100_000.
let vol = 0.55

let tree ?(steps = 200) ?(rate = 0.04) () =
  Density.lognormal ~spot0 ~horizon:expiry ~rate ~vol ()
  |> Replication.Model.of_density
  |> Lattice.create ~steps
  |> Or_error.ok_exn
;;

let t200 = tree ()

let%expect_test "the tree is a valid tree" =
  (* Up probabilities should sit near 0.5 wherever the price is likely to
     be: a roughly normal distribution on a binomial spacing means roughly
     even odds. Far-out nodes that carry almost no probability can have
     lopsided odds without mattering. *)
  let n = Lattice.steps t200 in
  let nodes =
    List.concat_map (List.range 0 n) ~f:(fun s ->
      List.init (s + 1) ~f:(fun j ->
        Lattice.node_probability t200 ~step:s ~node:j, Lattice.up_probability t200 ~step:s ~node:j))
  in
  let likely = List.filter_map nodes ~f:(fun (p, u) -> Option.some_if Float.(p > 1e-6) u) in
  let lopsided =
    List.sum (module Float) nodes ~f:(fun (p, u) -> if Float.(u < 0.05 || u > 0.95) then p else 0.)
  in
  printf "steps %d; price today %.2f (spot %.2f)\n" n (Lattice.node_price t200 ~step:0 ~node:0) spot0;
  printf "up probability at nodes with probability > 1e-6: %.3f to %.3f\n"
    (List.reduce_exn likely ~f:Float.min) (List.reduce_exn likely ~f:Float.max);
  printf "probability at nodes with up odds below 5%% or above 95%%: below 1e-9: %b\n"
    Float.(lopsided < 1e-9);
  let final_total =
    List.sum (module Float) (List.init (n + 1) ~f:Fn.id) ~f:(fun j ->
      Lattice.node_probability t200 ~step:n ~node:j)
  in
  printf "final-step probabilities sum to 1: %b\n" Float.(abs (final_total - 1.) < 1e-12);
  [%expect {|
    steps 200; price today 100000.00 (spot 100000.00)
    up probability at nodes with probability > 1e-6: 0.488 to 0.512
    probability at nodes with up odds below 5% or above 95%: below 1e-9: true
    final-step probabilities sum to 1: true
    |}]
;;

let%expect_test "receiving Spot at any step is worth today's spot" =
  (* Discounted prices are fair on the tree: E[S_t] e^{-rt} = S_0 for every t.
     If the tree drifted, this would show it. *)
  List.iter [ 0; 1; 50; 137; 200 ] ~f:(fun k ->
    let t = Float.of_int k *. Lattice.dt t200 in
    let c = Contract.at t (Contract.scale Spot Contract.one) in
    let gap = Float.round_decimal (Lattice.price_exn t200 c -. spot0) ~decimal_digits:6 +. 0. in
    printf "step %3d: value minus spot %.6f\n" k gap);
  [%expect {|
    step   0: value minus spot 0.000000
    step   1: value minus spot 0.000000
    step  50: value minus spot 0.000000
    step 137: value minus spot 0.000000
    step 200: value minus spot 0.000000
    |}]
;;

let%expect_test "European contracts: the tree agrees with the grid pricer" =
  (* At the horizon the tree's nodes are a density; the grid pricer on that
     density must give the same price for any contract that only looks at
     the horizon. *)
  let d = Lattice.terminal_density t200 in
  Test_laws.check "Lattice = Pricer on the terminal density" Test_laws.contract
    ~sexp_of:Contract.sexp_of_t ~f:(fun c ->
      Test_laws.close (Lattice.price_exn t200 c) (Pricer.price_exn d c));
  [%expect {| holds: Lattice = Pricer on the terminal density |}]
;;

let%expect_test "an American right is worth at least waiting or exercising now" =
  (* [anytime T c] can always mimic acquiring c at T, or acquiring it at
     once, so it is worth at least the better of the two. *)
  Test_laws.check "anytime T c >= max (at T c) (c now)" Test_laws.payoff
    ~sexp_of:Contract.sexp_of_t ~f:(fun c ->
      let american = Lattice.price_exn t200 (Contract.anytime expiry c)
      and european = Lattice.price_exn t200 (Contract.at expiry c)
      and now = Lattice.price_exn t200 c in
      Float.(american >= max european now - (1e-9 * (1. + abs american))));
  [%expect {| holds: anytime T c >= max (at T c) (c now) |}]
;;

let%expect_test "Merton: never exercise an American call early; a put, sometimes" =
  (* With a positive rate and no dividends, waiting is always worth more for
     a call, so the American call equals the European one. For a put, getting
     the strike early earns interest, so it can be worth exercising. At a
     zero rate neither has a reason to exercise early. *)
  List.iter [ 0.04; 0. ] ~f:(fun rate ->
    let t = tree ~rate () in
    let p c = Lattice.price_exn t c in
    let k = 100_000. in
    printf "rate %.2f: call  American %.2f  European %.2f\n" rate
      (p (Instruments.american_call ~expiry ~strike:k))
      (p (Instruments.call ~expiry ~strike:k));
    printf "rate %.2f: put   American %.2f  European %.2f\n" rate
      (p (Instruments.american_put ~expiry ~strike:k))
      (p (Instruments.put ~expiry ~strike:k)));
  [%expect {|
    rate 0.04: call  American 3072.54  European 3072.54
    rate 0.04: put   American 2999.88  European 2995.85
    rate 0.00: call  American 3036.33  European 3036.33
    rate 0.00: put   American 3036.33  European 3036.33
    |}]
;;

(* Closed form for geometric Brownian motion with zero rate: the chance that
   the price ever reaches B above S0 before T. With x = ln S_T having drift
   mu = -vol^2/2 and b = ln (B / S0):
     P = N((-b + mu T) / (vol sqrt T)) + (S0 / B) N((-b - mu T) / (vol sqrt T)) *)
let continuous_touch ~barrier =
  let n x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.) in
  let b = Float.log (barrier /. spot0)
  and mu = -.(vol *. vol) /. 2.
  and s = vol *. Float.sqrt expiry in
  n ((-.b +. (mu *. expiry)) /. s) +. (spot0 /. barrier *. n ((-.b -. (mu *. expiry)) /. s))
;;

let%expect_test "one-touch: tree vs the continuous formula and its discrete correction" =
  (* The tree checks the barrier only at its steps, so it touches less often
     than a continuously watched price. Broadie, Glasserman and Kou (1997):
     checking at m steps behaves like checking continuously with the
     barrier moved out by a factor exp(0.5826 vol sqrt(T / m)).

     The tree's answers also wobble with the step count, as the digital
     column shows against its exact value: the barrier and the strike fall
     at different places between nodes as the spacing changes, the same
     sawtooth as the digital tests in test_pricer.ml (for barriers this is
     Boyle and Lau, 1994). With enough steps both settle down. *)
  let barrier = 105_000. in
  let n x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.) in
  let exact_digital =
    n ((Float.log (spot0 /. barrier) -. (vol *. vol *. expiry /. 2.)) /. (vol *. Float.sqrt expiry))
  in
  printf "continuous one-touch formula: %.4f; exact digital: %.4f\n" (continuous_touch ~barrier)
    exact_digital;
  List.iter [ 50; 200; 800 ] ~f:(fun steps ->
    let t = tree ~steps ~rate:0. () in
    let corrected =
      continuous_touch
        ~barrier:(barrier *. Float.exp (0.5826 *. vol *. Float.sqrt (expiry /. Float.of_int steps)))
    in
    printf "steps %3d: tree touch %.4f vs corrected formula %.4f; tree digital %.4f\n" steps
      (Lattice.price_exn t (Instruments.one_touch_above ~expiry ~barrier))
      corrected
      (Lattice.price_exn t (Instruments.digital_above ~expiry ~strike:barrier)));
  [%expect {|
    continuous one-touch formula: 0.5091; exact digital: 0.2487
    steps  50: tree touch 0.4796 vs corrected formula 0.4568; tree digital 0.2389
    steps 200: tree touch 0.4805 vs corrected formula 0.4825; tree digital 0.2618
    steps 800: tree touch 0.4969 vs corrected formula 0.4957; tree digital 0.2502
    |}]
;;

let%expect_test "dates the tree can't use" =
  let show c =
    print_s [%sexp (Lattice.price t200 c : (float, Unpriceable.t) Result.t)]
  in
  let dt = Lattice.dt t200 in
  show (Contract.at (dt *. 10.5) Contract.one);
  show (Contract.at (expiry *. 2.) Contract.one);
  show (Contract.at (dt *. 10.) (Contract.at (dt *. 5.) Contract.one));
  [%expect {|
    (Error
     (Off_grid (requested 0.0010068493150684932) (step 9.5890410958904119E-05)))
    (Error
     (Unsupported_date (requested 0.038356164383561646)
      (model_horizon 0.019178082191780823)))
    (Error
     (Already_passed (date 0.0004794520547945206) (now 0.00095890410958904119)))
    |}]
;;

let%expect_test "real smile: touching a strike before the close vs finishing above it" =
  (* Kalshi's hourly "above" strikes, priced two ways from the same Implied
     Odds smile: finishing above at the close (the Kalshi contract) and
     touching the strike at any point before (a "reach" contract). For a
     strike above today's price, touching is roughly twice as likely as
     finishing above: once the price has touched, it is about as likely to
     end below as above. *)
  let pairs = Or_error.ok_exn (force Test_replication.sample) in
  let s, _ =
    List.find_exn pairs ~f:(fun ((s : Export.Surface.t), _) -> Export.Kind.equal s.kind Hourly)
  in
  let smile = s.smile in
  let expiry = Smile.years smile in
  let tree =
    Lattice.create ~steps:400 (Replication.Model.of_smile smile) |> Or_error.ok_exn
  in
  printf "forward %.0f\n" (Smile.forward smile);
  printf "%10s %10s %10s %7s\n" "strike" "above" "touch" "ratio";
  List.filter_map pairs ~f:(fun ((s' : Export.Surface.t), (r : Export.Row.t)) ->
    Option.some_if
      (String.equal s'.event s.event
       && Float.(r.strike > Smile.forward smile)
       && Float.(r.p_opt > 0.02))
      r.strike)
  |> List.sort ~compare:Float.compare
  |> List.iter ~f:(fun k ->
    let above = Lattice.price_exn tree (Instruments.digital_above ~expiry ~strike:k)
    and touch = Lattice.price_exn tree (Instruments.one_touch_above ~expiry ~barrier:k) in
    printf "%10.2f %10.3f %10.3f %7.2f\n" k above touch (touch /. above));
  [%expect {|
    forward 81983
        strike      above      touch   ratio
      81999.99      0.477      0.941    1.97
      82099.99      0.243      0.565    2.33
      82199.99      0.137      0.293    2.14
      82299.99      0.081      0.162    2.01
      82399.99      0.049      0.094    1.93
      82499.99      0.028      0.056    2.02
    |}]
;;
