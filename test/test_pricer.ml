(* Expect tests.

   Each test prints something and the expected output sits right below it in
   [%expect]. Run [dune runtest]: no output means everything matched. If the
   printed output changes, dune shows a diff; [dune promote] accepts it.

   Most tests check *identities* rather than hand-computed numbers. An
   identity like put-call parity must hold for any correct pricer, so when one
   breaks you know the bug is real, not a typo in an expected value. *)

open! Core
open Contracts

(* A BTC-like toy world: $100k spot, one-week horizon, 55% vol, 4% rate. *)
let spot0 = 100_000.
let expiry = 7. /. 365.
let rate = 0.04
let vol = 0.55
let model = Density.lognormal ~spot0 ~horizon:expiry ~rate ~vol ()
let price c = Pricer.price_exn model c

(* Round, and turn -0.000 into 0.000 so tiny negative noise prints cleanly. *)
let r ?(digits = 4) x = Float.round_decimal x ~decimal_digits:digits +. 0.

(* Closed-form Black-Scholes, used only to check the discretised density. *)
module Bs = struct
  let cdf z = 0.5 *. (1. +. Stdlib.Float.erf (z /. Float.sqrt 2.))
  let pdf z = Float.exp (-.(z *. z) /. 2.) /. Float.sqrt (2. *. Float.pi)
  let sd = vol *. Float.sqrt expiry

  let d2 k =
    (Float.log (spot0 /. k) +. ((rate -. (vol *. vol /. 2.)) *. expiry)) /. sd
  ;;

  let df = Float.exp (-.rate *. expiry)
  let call k = (spot0 *. cdf (d2 k +. sd)) -. (k *. df *. cdf (d2 k))
  let digital k = df *. cdf (d2 k)
  let density k = pdf (d2 k) /. (k *. sd)
end

let%expect_test "a contract is just data" =
  let c = Kalshi.above ~expiry ~strike:102_000. in
  print_s [%sexp (c : Contract.t)];
  [%expect {| (At 0.019178082191780823 (Cond (Ge Spot (Const 102000)) One Zero)) |}]
;;

let%expect_test "a second interpreter: which dates does a contract depend on?" =
  let one_week = expiry
  and two_weeks = 14. /. 365. in
  let c =
    Contract.and_
      (Instruments.call ~expiry:one_week ~strike:100_000.)
      (Instruments.digital_above ~expiry:two_weeks ~strike:110_000.)
  in
  print_s [%sexp (Contract.dates c : float list)];
  (* The density only knows about one week out, so the pricer refuses,
     and tells us exactly why. *)
  print_s [%sexp (Pricer.price model c : (float, Pricer.Unpriceable.t) Result.t)];
  [%expect {|
    (0.019178082191780823 0.038356164383561646)
    (Error
     (Unsupported_date (requested 0.038356164383561646)
      (model_horizon 0.019178082191780823)))
    |}]
;;

let%expect_test "bond and forward match closed forms" =
  let strike = 101_000. in
  printf "zero-coupon  model %.6f  closed form %.6f\n"
    (price (Instruments.zero_coupon ~expiry))
    (Density.discount model);
  printf "forward      model %.2f  closed form %.2f\n"
    (price (Instruments.forward ~expiry ~strike))
    (spot0 -. (strike *. Density.discount model));
  [%expect {|
    zero-coupon  model 0.999233  closed form 0.999233
    forward      model -922.55  closed form -922.55
    |}]
;;

let%expect_test "the discretised density reproduces Black-Scholes" =
  printf "%10s %12s %12s %10s %10s\n" "strike" "call" "BS call" "digital" "BS dig";
  List.iter [ 90_000.; 95_000.; 100_000.; 105_000.; 110_000. ] ~f:(fun k ->
    printf "%10.0f %12.2f %12.2f %10.4f %10.4f\n" k
      (price (Instruments.call ~expiry ~strike:k))
      (Bs.call k)
      (price (Instruments.digital_above ~expiry ~strike:k))
      (Bs.digital k));
  [%expect {|
    strike         call      BS call    digital     BS dig
     90000     10337.63     10337.63     0.9114     0.9116
     95000      6162.64      6162.64     0.7402     0.7401
    100000      3075.20      3075.21     0.4877     0.4885
    105000      1251.39      1251.39     0.2513     0.2517
    110000       411.18       411.18     0.1006     0.1003
    |}]
;;

let%expect_test "put-call parity holds exactly (the pricer is linear)" =
  List.iter [ 90_000.; 100_000.; 110_000. ] ~f:(fun strike ->
    let c = price (Instruments.call ~expiry ~strike)
    and p = price (Instruments.put ~expiry ~strike)
    and f = price (Instruments.forward ~expiry ~strike) in
    printf "K=%.0f  call - put - forward = %.6f\n" strike (r ~digits:6 (c -. p -. f)));
  [%expect {|
    K=90000  call - put - forward = 0.000000
    K=100000  call - put - forward = 0.000000
    K=110000  call - put - forward = 0.000000
    |}]
;;

let%expect_test "WHEN you choose matters" =
  let strike = 100_000. in
  let call = Instruments.call ~expiry ~strike
  and put = Instruments.put ~expiry ~strike in
  (* Choose today between owning the call or the put. *)
  let choose_today = Contract.or_ call put in
  (* Choose at expiry, once the price is known: that is a straddle. *)
  let choose_at_expiry =
    Contract.at
      expiry
      (Contract.or_
         (Contract.scale (Instruments.spot_minus strike) Contract.one)
         (Contract.scale (Instruments.minus_spot strike) Contract.one))
  in
  printf "call             %8.2f\n" (price call);
  printf "put              %8.2f\n" (price put);
  printf "choose today     %8.2f   (= max of the two)\n" (price choose_today);
  printf "choose at expiry %8.2f   (= call + put)\n" (price choose_at_expiry);
  [%expect {|
    call              3075.20
    put               2998.52
    choose today      3075.20   (= max of the two)
    choose at expiry  6073.72   (= call + put)
    |}]
;;

let%expect_test "a full Kalshi ladder prices to exactly $1 at expiry" =
  let edges = List.init 9 ~f:(fun i -> 92_000. +. (2_000. *. Float.of_int i)) in
  let ladder = Kalshi.ladder ~expiry ~edges in
  List.iter ladder ~f:(fun (label, c) -> printf "%-22s %.4f\n" label (price c));
  let total = List.sum (module Float) ladder ~f:(fun (_, c) -> price c) in
  printf "%-22s %.6f\n" "sum of buckets" total;
  printf "%-22s %.6f\n" "discount factor" (Density.discount model);
  [%expect {|
    below 92000            0.1432
    92000 to 94000         0.0727
    94000 to 96000         0.0890
    96000 to 98000         0.1008
    98000 to 100000        0.1059
    100000 to 102000       0.1020
    102000 to 104000       0.0919
    104000 to 106000       0.0802
    106000 to 108000       0.0639
    108000 or above        0.1496
    sum of buckets         0.999233
    discount factor        0.999233
    |}]
;;

let%expect_test "call spreads converge to the digital" =
  let strike = 102_000. in
  printf "digital                   %.4f\n"
    (price (Instruments.digital_above ~expiry ~strike));
  List.iter [ 5_000.; 1_000.; 200.; 50. ] ~f:(fun width ->
    let spread = Instruments.call_spread ~expiry ~lo:strike ~hi:(strike +. width) in
    printf "call spread / %6.0f      %.4f\n" width (price spread /. width));
  [%expect {|
    digital                   0.3856
    call spread /   5000      0.2757
    call spread /   1000      0.3622
    call spread /    200      0.3814
    call spread /     50      0.3850
    |}]
;;

let%expect_test "a butterfly recovers the density" =
  let h = 500. in
  printf "%10s %16s %16s\n" "strike" "fly / (h^2 df)" "true density";
  List.iter [ 95_000.; 100_000.; 105_000. ] ~f:(fun mid ->
    let fly = Instruments.butterfly ~expiry ~lo:(mid -. h) ~mid ~hi:(mid +. h) in
    printf "%10.0f %16.3e %16.3e\n" mid
      (price fly /. (h *. h *. Density.discount model))
      (Bs.density mid));
  [%expect {|
    strike   fly / (h^2 df)     true density
     95000        4.474e-05        4.477e-05
    100000        5.238e-05        5.236e-05
    105000        3.990e-05        3.989e-05
    |}]
;;

let%expect_test "Anytime: an American call is path-dependent, so the pricer refuses" =
  let strike = 100_000. in
  let exercise_payoff =
    Contract.or_ (Contract.scale (Instruments.spot_minus strike) Contract.one) Contract.zero
  in
  let american_call = Contract.anytime expiry exercise_payoff in
  print_s [%sexp (Contract.dates american_call : float list)];
  print_s [%sexp (Pricer.price model american_call : (float, Pricer.Unpriceable.t) Result.t)];
  [%expect {|
    (0.019178082191780823)
    (Error (Path_dependent (deadline 0.019178082191780823)))
    |}]
;;

let%expect_test "Anytime at its own deadline collapses to the inner contract" =
  (* Acquire, at expiry, the right to exercise "any time up to expiry". By then
     only one moment is left, so this is exactly the European call. *)
  let strike = 100_000. in
  let exercise_payoff =
    Contract.or_ (Contract.scale (Instruments.spot_minus strike) Contract.one) Contract.zero
  in
  let collapsed = Contract.at expiry (Contract.anytime expiry exercise_payoff) in
  printf "collapsed anytime %.2f\n" (price collapsed);
  printf "european call     %.2f\n" (price (Instruments.call ~expiry ~strike));
  [%expect {|
    collapsed anytime 3075.20
    european call     3075.20
    |}]
;;

let%expect_test "above + below at the same strike is worth exactly $1 at expiry" =
  (* Every outcome lands in exactly one of the two, so the pair pays $1 for
     sure. Its price must therefore be the discount factor, at any strike and
     under any density. A strike far in either tail is included on purpose. *)
  List.iter [ 60_000.; 99_000.; 100_000.; 101_250.; 150_000. ] ~f:(fun strike ->
    let pair =
      Contract.and_ (Kalshi.above ~expiry ~strike) (Kalshi.below ~expiry ~strike)
    in
    printf "K=%-7.0f above %.4f  below %.4f  pair - df = %.9f\n" strike
      (price (Kalshi.above ~expiry ~strike))
      (price (Kalshi.below ~expiry ~strike))
      (r ~digits:9 (price pair -. Density.discount model)));
  [%expect {|
    K=60000   above 0.9992  below 0.0000  pair - df = 0.000000000
    K=99000   above 0.5402  below 0.4590  pair - df = 0.000000000
    K=100000  above 0.4877  below 0.5116  pair - df = 0.000000000
    K=101250  above 0.4243  below 0.5749  pair - df = 0.000000000
    K=150000  above 0.0000  below 0.9992  pair - df = 0.000000000
    |}]
;;

let%expect_test "a third interpreter: contracts in English" =
  let show c = print_endline (Contract.to_english c) in
  show (Kalshi.above ~expiry ~strike:102_000.);
  show (Kalshi.ladder ~expiry ~edges:[ 98_000.; 100_000. ] |> List.hd_exn |> snd);
  show (Instruments.call ~expiry ~strike:100_000.);
  show (Instruments.call_spread ~expiry ~lo:100_000. ~hi:105_000.);
  [%expect {|
    at day 7: if Spot >= 102000 then receive $1 else nothing
    at day 7: if Spot < 98000 then receive $1 else nothing
    at day 7: the better of [receive $(Spot - 100000)] or nothing
    both [at day 7: the better of [receive $(Spot - 100000)] or nothing] and [the other side of [at day 7: the better of [receive $(Spot - 105000)] or nothing]]
    |}]
;;

let%expect_test "loading a density from CSV" =
  let load lines =
    Density.of_csv_lines ~spot0:100. ~horizon:expiry ~rate:0. lines
  in
  let show_points d = print_s [%sexp (Density.points d : (float * float) array)] in
  (* A three-point density, with a header, a comment and a blank line. *)
  (match
     load [ "price,probability"; "# toy"; "90, 0.25"; ""; "100,0.5"; "110,0.25" ]
   with
   | Ok d ->
     show_points d;
     printf "forward = %.2f\n"
       (Pricer.price_exn d (Instruments.forward ~expiry ~strike:0.))
   | Error e -> print_s [%sexp (e : Error.t)]);
  (* Each kind of bad input fails with a message that says why and where. *)
  List.iter
    [ [ "90,0.5"; "100;0.5" ]
    ; [ "90,0.5"; "abc,0.5" ]
    ; [ "90,0.5"; "100,0.4" ]
    ; [ "90,0.6"; "100,-0.1"; "110,0.5" ]
    ; []
    ]
    ~f:(fun lines ->
      match load lines with
      | Ok _ -> print_endline "unexpectedly loaded"
      | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect {|
    ((90 0.25) (100 0.5) (110 0.25))
    forward = 100.00
    ("expected two comma-separated fields" (line_number 2) (line "100;0.5"))
    ("expected two numbers" (line_number 2) (line abc,0.5))
    ("probabilities do not sum to 1" (total 0.9) (tolerance 1E-06))
    ("price must be positive and probability non-negative" (line_number 2)
     (line 100,-0.1))
    "density has no points"
    |}]
;;

let%expect_test "loading a density from a file round-trips" =
  let file = Stdlib.Filename.temp_file "density" ".csv" in
  Out_channel.write_lines file [ "95,0.3"; "100,0.4"; "105,0.3" ];
  (match Density.of_csv ~spot0:100. ~horizon:expiry ~rate:0. file with
   | Ok d -> print_s [%sexp (Array.length (Density.points d) : int)]
   | Error e -> print_s [%sexp (e : Error.t)]);
  (match Density.of_csv ~spot0:100. ~horizon:expiry ~rate:0. (file ^ ".missing") with
   | Ok _ -> print_endline "unexpectedly loaded"
   | Error _ -> print_endline "missing file -> Error");
  Stdlib.Sys.remove file;
  [%expect {|
    3
    missing file -> Error
    |}]
;;

(* Why do digitals miss Black-Scholes in the 4th decimal when calls match to
   the cent?

   The density is a grid: all the probability inside a bin sits on one point.
   A digital pays 1 on one side of the strike and 0 on the other, so the bin
   that contains the strike is counted either wholly in or wholly out. The
   error is therefore some fraction of one bin's probability, and it depends
   on where the strike falls inside that bin: it saws up and down as the
   strike slides across the bin (first test below), and it shrinks in
   proportion to the bin width (second test: 4x the points, 1/4 the error).

   A call's payoff is continuous: just past the strike it pays almost
   nothing, so misplacing mass inside the strike's bin costs almost nothing.
   Away from the strike the payoff is linear, and a point placed at the bin's
   conditional mean prices a linear payoff exactly. That is why a call is
   within a few cents of Black-Scholes even on the coarsest grid, and its
   error falls faster than the digital's as the grid gets finer.

   The practical lesson for Milestone 2: on a grid, price a digital from
   smooth call prices (minus the slope of the call curve, Breeden-Litzenberger
   as Implied Odds' bl.py does), not by summing grid points, or put Kalshi's
   strikes on bin edges. *)

let%expect_test "digital error saws across one grid bin" =
  (* Near the money each bin is about $30 wide at n = 4001. *)
  printf "%9s %14s\n" "strike" "error x 1e4";
  List.init 8 ~f:(fun i -> 100_000. +. (5. *. Float.of_int i))
  |> List.iter ~f:(fun k ->
    printf "%9.0f %14.2f\n" k
      (1e4 *. (price (Instruments.digital_above ~expiry ~strike:k) -. Bs.digital k)));
  [%expect {|
    strike    error x 1e4
    100000          -7.89
    100005          -5.28
    100010          -2.66
    100015          -0.05
    100020           2.57
    100025           5.18
    100030           7.80
    100035          -5.52
    |}]
;;

let%expect_test "digital error shrinks in proportion to bin width; call error stays tiny" =
  printf "%7s %24s %18s\n" "points" "max |digital error|" "|ATM call error|";
  List.iter [ 1001; 4001; 16001 ] ~f:(fun n ->
    let m = Density.lognormal ~n ~spot0 ~horizon:expiry ~rate ~vol () in
    let p c = Pricer.price_exn m c in
    let max_digital_error =
      List.init 100 ~f:(fun i -> 99_500. +. (10. *. Float.of_int i))
      |> List.map ~f:(fun k ->
        Float.abs (p (Instruments.digital_above ~expiry ~strike:k) -. Bs.digital k))
      |> List.reduce_exn ~f:Float.max
    in
    let call_error =
      Float.abs (p (Instruments.call ~expiry ~strike:100_000.) -. Bs.call 100_000.)
    in
    printf "%7d %24.2e %18.4f\n" n max_digital_error call_error);
  [%expect {|
    points      max |digital error|   |ATM call error|
      1001                 3.18e-03             0.0247
      4001                 7.89e-04             0.0060
     16001                 1.97e-04             0.0004
    |}]
;;

let%expect_test "Anytime and At: every way a date can relate to now and the horizon" =
  let show name c =
    printf "%-44s %s\n" name
      (match Pricer.price model c with
       | Ok v -> sprintf "%.4f" v
       | Error e -> Sexp.to_string [%sexp (e : Pricer.Unpriceable.t)])
  in
  let half = expiry /. 2.
  and twice = expiry *. 2. in
  show "anytime 0 one (deadline is now)" (Contract.anytime 0. Contract.one);
  show "anytime half one (window open)" (Contract.anytime half Contract.one);
  show "anytime twice one (beyond horizon)" (Contract.anytime twice Contract.one);
  show "at T (anytime T one)" (Contract.at expiry (Contract.anytime expiry Contract.one));
  show "at T (anytime half one) (already passed)"
    (Contract.at expiry (Contract.anytime half Contract.one));
  show "at T (anytime twice one) (beyond horizon)"
    (Contract.at expiry (Contract.anytime twice Contract.one));
  show "at 0 one (acquire now)" (Contract.at 0. Contract.one);
  show "at T (at half one) (already passed)" (Contract.at expiry (Contract.at half Contract.one));
  (* Anytime only makes the timing optional: you must still acquire the
     inner contract, so an obligation to pay $1 stays an obligation. *)
  show "at T (anytime T (give one))"
    (Contract.at expiry (Contract.anytime expiry (Contract.give Contract.one)));
  [%expect {|
    anytime 0 one (deadline is now)              1.0000
    anytime half one (window open)               (Path_dependent(deadline 0.0095890410958904115))
    anytime twice one (beyond horizon)           (Unsupported_date(requested 0.038356164383561646)(model_horizon 0.019178082191780823))
    at T (anytime T one)                         0.9992
    at T (anytime half one) (already passed)     (Already_passed(date 0.0095890410958904115)(now 0.019178082191780823))
    at T (anytime twice one) (beyond horizon)    (Unsupported_date(requested 0.038356164383561646)(model_horizon 0.019178082191780823))
    at 0 one (acquire now)                       1.0000
    at T (at half one) (already passed)          (Already_passed(date 0.0095890410958904115)(now 0.019178082191780823))
    at T (anytime T (give one))                  -0.9992
    |}]
;;

let%expect_test "to_english keeps real Kalshi strikes and hourly times exact" =
  let one_hour = 1. /. (365. *. 24.) in
  print_endline (Contract.to_english (Kalshi.above ~expiry:one_hour ~strike:101_999.99));
  print_endline (Contract.to_english (Kalshi.above ~expiry ~strike:102_000.5));
  List.iter (Kalshi.ladder ~expiry ~edges:[ 92_249.99; 92_499.99 ]) ~f:(fun (label, _) ->
    print_endline label);
  [%expect {|
    at hour 1: if Spot >= 101999.99 then receive $1 else nothing
    at day 7: if Spot >= 102000.5 then receive $1 else nothing
    below 92249.99
    92249.99 to 92499.99
    92499.99 or above
    |}]
;;

let%expect_test "CSV edge cases from real files" =
  let load ?tolerance lines =
    match Density.of_csv_lines ?tolerance ~spot0:100. ~horizon:expiry ~rate:0. lines with
    | Ok d -> printf "ok, %d points\n" (Array.length (Density.points d))
    | Error e -> print_s [%sexp (e : Error.t)]
  in
  (* Header with a space and a byte-order mark; Windows line endings;
     scientific notation. *)
  load [ "\xEF\xBB\xBFprice, probability\r"; "9e1,0.25\r"; "100,0.5\r"; "110,2.5e-1\r" ];
  (* Hex, digit separators and a negative probability are rejected, and
     every bad line is reported with its number. *)
  load [ "0x5A,0.25"; "100,0.5"; "1_00,0.25"; "120,-0.1" ];
  (* Three thirds rounded to 4 places sum to 0.9999: rejected by default,
     accepted with a looser tolerance. *)
  load [ "90,0.3333"; "100,0.3333"; "110,0.3333" ];
  load ~tolerance:1e-3 [ "90,0.3333"; "100,0.3333"; "110,0.3333" ];
  (* NaN would slip past "p < 0" because NaN fails every comparison. *)
  print_s
    [%sexp
      (Density.create ~spot0:100. ~horizon:expiry ~rate:0. [ 100., Float.nan ]
       |> Result.is_error
       : bool)];
  [%expect {|
    ok, 3 points
    (("expected two numbers" (line_number 1) (line 0x5A,0.25))
     ("expected two numbers" (line_number 3) (line 1_00,0.25))
     ("price must be positive and probability non-negative" (line_number 4)
      (line 120,-0.1)))
    ("probabilities do not sum to 1" (total 0.9999) (tolerance 1E-06))
    ok, 3 points
    true
    |}]
;;
