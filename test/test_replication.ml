(* The replication pricer and the Implied Odds export.

   Three things are checked here. Payoffs: a contract compiles to the
   piecewise-linear function you would draw for it. Agreement: on the grid
   density, replication and the grid pricer give the same price for random
   contracts, so neither has a bug the other lacks. Real data: Kalshi
   contracts, written in the contract language and priced from Implied Odds'
   smiles, reproduce Implied Odds' own probabilities (p_opt) to 1e-9. *)

open! Core
open Contracts

let expiry = 7. /. 365.

let show_payoff c =
  match Payoff.of_contract ~horizon:expiry c with
  | Ok f ->
    List.iter f ~f:(fun { lo; hi; a; b } ->
      let edge x = if Float.is_finite x then sprintf "%.0f" x else "inf" in
      printf "  (%s, %s): %g + %g S\n" (edge lo) (edge hi) a b)
  | Error e -> print_s [%sexp (e : Payoff.Error.t)]
;;

let%expect_test "contracts compile to the payoff you would draw" =
  print_endline "digital above 100000:";
  show_payoff (Kalshi.above ~expiry ~strike:100_000.);
  print_endline "call at 100000 (the kink appears where the two lines cross):";
  show_payoff (Instruments.call ~expiry ~strike:100_000.);
  print_endline "butterfly 99000 / 100000 / 101000:";
  show_payoff (Instruments.butterfly ~expiry ~lo:99_000. ~mid:100_000. ~hi:101_000.);
  print_endline "range 98000 to 100000:";
  show_payoff (Instruments.range ~expiry ~lo:98_000. ~hi:100_000.);
  print_endline "receive Spot * Spot (a curve, not lines):";
  show_payoff
    (Contract.at expiry (Contract.scale (Contract.Obs.Mul (Spot, Spot)) Contract.one));
  [%expect {|
    digital above 100000:
      (0, 100000): 0 + 0 S
      (100000, inf): 1 + 0 S
    call at 100000 (the kink appears where the two lines cross):
      (0, 100000): 0 + 0 S
      (100000, inf): -100000 + 1 S
    butterfly 99000 / 100000 / 101000:
      (0, 99000): 0 + 0 S
      (99000, 100000): -99000 + 1 S
      (100000, 101000): 101000 + -1 S
      (101000, inf): 0 + 0 S
    range 98000 to 100000:
      (0, 98000): 0 + 0 S
      (98000, 100000): 1 + 0 S
      (100000, inf): 0 + 0 S
    receive Spot * Spot (a curve, not lines):
    (Nonlinear (Mul Spot Spot))
    |}]
;;

(* --- Agreement with the grid pricer on random contracts ------------------ *)

module G = Quickcheck.Generator

let model = Density.lognormal ~n:401 ~spot0:100_000. ~horizon:expiry ~rate:0.04 ~vol:0.55 ()
let replication = Replication.Model.of_density model

(* Random contracts as in test_laws.ml, plus dates that are now, the
   horizon, before it or beyond it, and Anytime windows, so the date rules
   of both pricers are compared too. *)
let date = G.of_list [ 0.; expiry /. 2.; expiry; expiry *. 2. ]

let dated : Contract.t G.t =
  let open G.Let_syntax in
  G.union
    [ Test_laws.contract
    ; (let%map t = date
       and c = Test_laws.payoff in
       Contract.at t c)
    ; (let%map t = date
       and c = Test_laws.payoff in
       Contract.anytime t c)
    ; (let%map t = date
       and c = Test_laws.payoff
       and d = Test_laws.contract in
       Contract.and_ d (Contract.at expiry (Contract.at t c)))
    ]
;;

let agree c =
  match Pricer.price model c, Replication.price replication c with
  | Ok x, Ok y -> Test_laws.close x y
  | Error e1, Error (Payoff.Error.Unpriceable e2) -> Pricer.Unpriceable.equal e1 e2
  | _ -> false
;;

let%expect_test "replication agrees with the grid pricer on random contracts" =
  Test_laws.check "Replication = Pricer (values and errors)" dated
    ~sexp_of:Contract.sexp_of_t ~f:agree;
  [%expect {| holds: Replication = Pricer (values and errors) |}]
;;

(* --- The Implied Odds export --------------------------------------------- *)

let sample =
  lazy
    (let open Or_error.Let_syntax in
     let%bind surfaces = Export.load_surfaces "data/surfaces_sample.csv"
     and rows = Export.load_rows "data/contracts_sample.csv" in
     Export.join surfaces rows)
;;

(* Kalshi's "BTC above strike at close", written in the contract language and
   priced by replication from that event's smile. *)
let model_price ((s : Export.Surface.t), (r : Export.Row.t)) =
  let smile = s.smile in
  Replication.price_exn
    (Replication.Model.of_smile smile)
    (Kalshi.above ~expiry:(Smile.years smile) ~strike:r.strike)
;;

let%expect_test "Kalshi contracts priced in OCaml reproduce p_opt to 1e-9" =
  let pairs = Or_error.ok_exn (force sample) in
  let errors = List.map pairs ~f:(fun ((_, r) as pair) -> Float.abs (model_price pair -. r.p_opt)) in
  let worst = List.reduce_exn errors ~f:Float.max in
  printf "contracts: %d\n" (List.length pairs);
  printf "every one within 1e-9 of p_opt: %b\n" Float.(worst < 1e-9);
  [%expect {|
    contracts: 238
    every one within 1e-9 of p_opt: true
    |}]
;;

let%expect_test "near the money: the language's price equals p_opt" =
  (* Pricing only. Kalshi's quotes for this window are kept out of every
     test: comparing them with the model is Implied Odds' sealed layer-5
     analysis, which opens on 2026-11-30. *)
  let pairs = Or_error.ok_exn (force sample) in
  printf "%-8s %10s %8s %8s\n" "kind" "strike" "p_opt" "model";
  List.filter pairs ~f:(fun (_, r) -> Float.(r.p_opt > 0.1 && r.p_opt < 0.9))
  |> List.iter ~f:(fun ((s, r) as pair) ->
    printf "%-8s %10.2f %8.4f %8.4f\n"
      (Sexp.to_string [%sexp (s.kind : Export.Kind.t)])
      r.strike r.p_opt (model_price pair));
  [%expect {|
    kind         strike    p_opt    model
    Hourly     82199.99   0.1336   0.1336
    Hourly     82099.99   0.2391   0.2391
    Hourly     81999.99   0.4725   0.4725
    Hourly     81899.99   0.7553   0.7553
    Hourly     81799.99   0.8624   0.8624
    Daily      82999.99   0.1759   0.1759
    Daily      82499.99   0.3204   0.3204
    Daily      81999.99   0.5107   0.5107
    Daily      81499.99   0.6960   0.6960
    Daily      80999.99   0.8303   0.8303
    |}]
;;

(* Bucket prices telescope: each edge's P(S >= K) is added once and
   subtracted once, so the total is 1 whatever the smile. This checks the
   ladder construction and the replication bookkeeping, not the smile. *)
let%expect_test "a full ladder on a real smile's strikes telescopes to exactly 1" =
  let pairs = Or_error.ok_exn (force sample) in
  let s, _ = List.hd_exn pairs in
  let smile = s.smile in
  let edges =
    List.filter_map pairs ~f:(fun ((s', r) : Export.Surface.t * Export.Row.t) ->
      Option.some_if (String.equal s'.event s.event) r.strike)
    |> List.dedup_and_sort ~compare:Float.compare
  in
  let total =
    Kalshi.ladder ~expiry:(Smile.years smile) ~edges
    |> List.sum (module Float) ~f:(fun (_, c) ->
      Replication.price_exn (Replication.Model.of_smile smile) c)
  in
  printf "%d buckets, total %.12f\n" (List.length edges + 1) total;
  [%expect {| 189 buckets, total 1.000000000000 |}]
;;

(* --- Regressions from review --------------------------------------------- *)

let%expect_test "breakpoints one float apart don't break the payoff" =
  (* A call crossing computed as 0.1k / 0.1 lands one float below k, right
     next to a digital's breakpoint at k. *)
  let k = 90_001.48 in
  let call_like =
    Contract.or_
      (Contract.scale_k 0.1 (Contract.scale (Instruments.spot_minus k) Contract.one))
      Contract.zero
  in
  let c =
    Contract.at expiry
      (Contract.and_ call_like (Contract.cond (Ge (Spot, Const k)) Contract.one Contract.zero))
  in
  let two_digitals =
    Contract.and_ (Kalshi.above ~expiry ~strike:k)
      (Kalshi.above ~expiry ~strike:(Float.one_ulp `Up k))
  in
  List.iter [ "call + digital at adjacent floats", c; "two digitals one float apart", two_digitals;
              "digital at 1e308", Kalshi.above ~expiry ~strike:1e308 ]
    ~f:(fun (name, c) ->
      printf "%-34s grid %10.4f  replication %10.4f\n" name (Pricer.price_exn model c)
        (Replication.price_exn replication c));
  [%expect {|
    call + digital at adjacent floats  grid  1034.5179  replication  1034.5179
    two digitals one float apart       grid     1.8173  replication     1.8173
    digital at 1e308                   grid     0.0000  replication     0.0000
    |}]
;;

let%expect_test "a branch no price can reach doesn't make a contract unpriceable" =
  (* Spot >= 0 always holds, so the [at 0] branch is never taken. *)
  let c = Contract.at expiry (Contract.cond (Ge (Spot, Const 0.)) Contract.one (Contract.at 0. Contract.one)) in
  (* Six decimals: enough to compare, few enough to print the same on any
     machine's maths library. *)
  let show = function
    | Ok v -> sprintf "Ok %.6f" v
    | Error _ -> "Error"
  in
  printf "grid:        %s\n" (show (Pricer.price model c));
  printf "replication: %s\n" (show (Replication.price replication c));
  [%expect {|
    grid:        Ok 0.999233
    replication: Ok 0.999233
    |}]
;;

let%expect_test "an atom exactly on a strike is counted the same way by both pricers" =
  (* A CSV density with round prices, and strikes exactly on those prices:
     the case where "S >= K" and "S > K" differ by a whole point's
     probability. Includes "S <= K" (a predicate with the price on the right),
     whose cut has to sit just above K. *)
  let d =
    Density.of_csv_lines ~spot0:100_000. ~horizon:expiry ~rate:0.
      [ "90000,0.25"; "100000,0.5"; "110000,0.25" ]
    |> Or_error.ok_exn
  in
  let r = Replication.Model.of_density d in
  List.iter
    [ "above 100000", Kalshi.above ~expiry ~strike:100_000.
    ; "below 100000", Kalshi.below ~expiry ~strike:100_000.
    ; "range 100000 to 110000", Instruments.range ~expiry ~lo:100_000. ~hi:110_000.
    ; "S <= 100000", Contract.at expiry (Contract.cond (Ge (Const 100_000., Spot)) Contract.one Contract.zero)
    ; "S > 100000", Contract.at expiry (Contract.cond (Lt (Const 100_000., Spot)) Contract.one Contract.zero)
    ; "call at 100000", Instruments.call ~expiry ~strike:100_000.
    ]
    ~f:(fun (name, c) ->
      printf "%-24s grid %.4f  replication %.4f\n" name (Pricer.price_exn d c)
        (Replication.price_exn r c));
  [%expect {|
    above 100000             grid 0.7500  replication 0.7500
    below 100000             grid 0.2500  replication 0.2500
    range 100000 to 110000   grid 0.5000  replication 0.5000
    S <= 100000              grid 0.7500  replication 0.7500
    S > 100000               grid 0.2500  replication 0.2500
    call at 100000           grid 2500.0000  replication 2500.0000
    |}]
;;

let%expect_test "a smile gives sensible answers at extreme strikes" =
  let pairs = Or_error.ok_exn (force sample) in
  let s, _ = List.hd_exn pairs in
  List.iter [ 0.; 5.; 1_000.; 1e7 ] ~f:(fun k ->
    (* [+. 0.] turns the -0 of a tiny negative rounding residue into 0. *)
    let p = Float.round_decimal (Smile.prob_above s.smile k) ~decimal_digits:6 +. 0. in
    printf "P(S > %g) = %.6f\n" k p);
  [%expect {|
    P(S > 0) = 1.000000
    P(S > 5) = 1.000000
    P(S > 1000) = 1.000000
    P(S > 1e+07) = 0.000000
    |}]
;;

(* The agreement law above never puts a dated contract inside a [Cond] or
   [Or] branch under [At]. Here it does. Which error gets reported can then
   legitimately differ (see replication.mli), so this law only asks that
   both pricers agree on WHETHER a price exists, and on the price if so. *)
let nested : Contract.t G.t =
  let open G.Let_syntax in
  let dated_leaf =
    let%map t = date
    and c = Test_laws.payoff in
    Contract.at t c
  in
  let branch = G.union [ Test_laws.payoff; dated_leaf ] in
  let%map p = Test_laws.pred
  and x = branch
  and y = branch
  and use_or = G.bool in
  Contract.at expiry (if use_or then Contract.or_ x y else Contract.cond p x y)
;;

let%expect_test "with dated branches, the pricers agree on whether and what price" =
  Test_laws.check "same Ok/Error, same value" nested ~sexp_of:Contract.sexp_of_t
    ~f:(fun c ->
      match Pricer.price model c, Replication.price replication c with
      | Ok x, Ok y -> Test_laws.close x y
      | Error _, Error _ -> true
      | _ -> false);
  [%expect {| holds: same Ok/Error, same value |}]
;;

let%expect_test "spotting a near-sharp kink in a smile" =
  (* The sample's smiles are smooth; the same smile with s squeezed to 4e-6
     (as some exported hourly fits are) has a kink at forward * e^m. *)
  let pairs = Or_error.ok_exn (force sample) in
  List.dedup_and_sort pairs ~compare:(fun ((a : Export.Surface.t), _) (b, _) ->
    String.compare a.event b.event)
  |> List.iter ~f:(fun ((s : Export.Surface.t), _) ->
    print_s [%sexp (s.event : string), (Smile.kink s.smile : float option)]);
  let s, _ = List.hd_exn pairs in
  let p = Smile.params s.smile in
  let kinked =
    Smile.create ~spot:(Smile.spot s.smile) ~forward:100_000. ~years:(Smile.years s.smile)
      ~k_min:(-0.1) ~k_max:0.1 { p with s = 4e-6; m = 0.001 }
    |> Or_error.ok_exn
  in
  printf "kinked smile: kink at %.0f\n" (Option.value_exn (Smile.kink kinked));
  [%expect {|
    (KXBTCD-26OCT1523 ())
    (KXBTCD-26OCT1617 ())
    kinked smile: kink at 100100
    |}]
;;
