(* The analysis pieces, on hand-checkable inputs and on the export sample. *)

open! Core
open Contracts

let%expect_test "Kalshi's taker fee rounds each order's total up to the cent" =
  (* By hand: 0.07 * 0.5 * 0.5 = 0.0175 per contract. 100 contracts cost
     $1.75 exactly; 1 contract costs 1.75 cents, rounded up to 2. At 1 cent
     a contract's raw fee is 0.0693 cents, so one contract still pays a
     whole cent, while 100 pay 7 cents. *)
  List.iter [ 0.5, 100; 0.5, 1; 0.01, 1; 0.01, 100; 0.55, 100; 0.65, 3 ]
    ~f:(fun (price, contracts) ->
      printf "price %.2f x %3d: %.4f per contract\n" price contracts
        (Analysis.taker_fee ~price ~contracts));
  [%expect {|
    price 0.50 x 100: 0.0175 per contract
    price 0.50 x   1: 0.0200 per contract
    price 0.01 x   1: 0.0100 per contract
    price 0.01 x 100: 0.0007 per contract
    price 0.55 x 100: 0.0174 per contract
    price 0.65 x   3: 0.0167 per contract
    |}]
;;

let%expect_test "edges at the touch" =
  let show = Option.value_map ~default:"none" ~f:(sprintf "%+.4f") in
  (* Model says 60%, ask is 55c: 5c of expected value, minus a 1.74c fee. *)
  printf "buy at 0.55, q 0.60:  %s\n"
    (show (Analysis.edge_buy_yes ~q:0.6 ~ask:(Some 0.55) ~contracts:100));
  printf "sell at 0.55, q 0.60: %s\n"
    (show (Analysis.edge_sell_yes ~q:0.6 ~bid:(Some 0.55) ~contracts:100));
  printf "sell at a zero bid:   %s\n"
    (show (Analysis.edge_sell_yes ~q:0.01 ~bid:(Some 0.) ~contracts:100));
  printf "no ask:               %s\n"
    (show (Analysis.edge_buy_yes ~q:0.6 ~ask:None ~contracts:100));
  [%expect {|
    buy at 0.55, q 0.60:  +0.0326
    sell at 0.55, q 0.60: -0.0674
    sell at a zero bid:   none
    no ask:               none
    |}]
;;

let%expect_test "ladder quantiles flatten a price that rises with the strike" =
  (* 120 is quoted above 110, which can't be right for "above K"; it is
     treated as 0.60. The 50% crossing is then between 120 and 130, found on
     the normal-quantile scale: probit(0.6) = 0.253, probit(0.5) = 0,
     probit(0.2) = -0.842, so it lies 0.253 / 1.095 = 23.1% of the way from
     120 to 130. A straight line in price would say 25%, i.e. 122.5. *)
  let ladder = [ 100., 0.9; 110., 0.6; 120., 0.65; 130., 0.2 ] in
  List.iter [ 0.5; 0.75; 0.95 ] ~f:(fun p ->
    printf "P = %.2f at strike %s\n" p
      (Option.value_map (Analysis.ladder_quantile ladder p) ~default:"none" ~f:(sprintf "%.1f")));
  [%expect {|
    P = 0.50 at strike 122.3
    P = 0.75 at strike 105.9
    P = 0.95 at strike none
    |}]
;;

let%expect_test "Report.quantile interpolates between order statistics" =
  printf "%.2f %.2f %.2f\n"
    (Report.quantile [ 4.; 1.; 3.; 2. ] 0.5)
    (Report.quantile [ 4.; 1.; 3.; 2. ] 0.25)
    (Report.quantile [ 7. ] 0.9);
  [%expect {| 2.50 1.75 7.00 |}]
;;

let sample =
  lazy
    (Or_error.ok_exn
       (let open Or_error.Let_syntax in
        let%bind surfaces = Export.load_surfaces "data/surfaces_sample.csv"
        and rows = Export.load_rows "data/contracts_sample.csv" in
        Export.join surfaces rows))
;;

let%expect_test "the model quantile inverts the model probability" =
  let s, _ = List.hd_exn (force sample) in
  let worst =
    List.map [ 0.1; 0.25; 0.5; 0.75; 0.9 ] ~f:(fun p ->
      Float.abs (Smile.prob_above s.smile (Smile.quantile_above s.smile p) -. p))
    |> List.reduce_exn ~f:Float.max
  in
  printf "P(S > quantile(p)) within 1e-9 of p: %b\n" Float.(worst < 1e-9);
  [%expect {| P(S > quantile(p)) within 1e-9 of p: true |}]
;;


let%expect_test "probit inverts the normal CDF" =
  List.iter [ 0.025; 0.25; 0.5; 0.75; 0.975 ] ~f:(fun p ->
    (* Round, then [+. 0.]: a result of -0.00000001 must print the same on
       every machine. *)
    printf "probit %.3f = %+.4f\n" p (Float.round_decimal (Analysis.probit p) ~decimal_digits:4 +. 0.));
  [%expect {|
    probit 0.025 = -1.9600
    probit 0.250 = -0.6745
    probit 0.500 = +0.0000
    probit 0.750 = +0.6745
    probit 0.975 = +1.9600
    |}]
;;

