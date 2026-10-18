(* The gap analysis on synthetic markets, where the answer is known.

   Implied Odds layer 5's verdict on the real window is sealed until
   2026-11-30, so the analysis code is tested here, on quotes generated from
   a planted truth, and run on the real window only after that date. Each
   test plants something (nothing, a wider distribution, a shifted one) and
   checks the analysis reports it, through the same [Report] functions
   [bin/analyze.ml] uses. *)

open! Core
open Contracts

let markets = [ Synthetic.daily; Synthetic.hourly ]
let kind_name k = Sexp.to_string (Export.Kind.sexp_of_t k)

let run ?half_spread ~kalshi () =
  List.concat_map markets ~f:(fun market -> Synthetic.pairs ?half_spread market ~kalshi)
  |> Report.analyze ~contracts:100
;;

let print_shapes rows =
  List.iter (Report.shape_summary (Report.shapes rows)) ~f:(fun (s : Report.Shape.t) ->
    printf "%-6s %2d snapshots: width ratio %.3f (IQR %.3f-%.3f), median shift %+6.0f\n"
      (kind_name s.kind) s.snapshots s.median_iqr_ratio s.iqr_ratio_p25 s.iqr_ratio_p75
      s.median_shift)
;;

let print_buckets rows =
  List.iter (Report.buckets rows) ~f:(fun (b : Report.Bucket.t) ->
    printf "%-6s q %.2f-%.2f: n %3d, mean gap %+.4f\n" (kind_name b.kind) b.lo b.hi b.n
      b.mean_gap)
;;

let print_actionable rows =
  match Report.actionable ~contracts:100 rows with
  | [] -> printf "no actionable quotes\n"
  | xs ->
    List.iter xs ~f:(fun (a : Report.Actionable.t) ->
      printf "%-6s %s: %d quotes, mean edge %+.4f\n" (kind_name a.kind)
        (match a.side with `Buy_yes -> "buy YES " | `Sell_yes -> "sell YES")
        a.rows a.mean_edge)
;;

let%expect_test "null: Kalshi trades on the model itself" =
  (* With nothing planted, widths match (ratio 1 up to tick rounding), every
     bucket's mean gap is within the half-cent rounding of the mid, and
     no quote beats spread plus fee. *)
  let rows = run ~kalshi:(Model_choice.model Smile) () in
  print_shapes rows;
  print_buckets rows;
  print_actionable rows;
  [%expect {|
    Hourly 20 snapshots: width ratio 1.000 (IQR 0.996-1.005), median shift     -2
    Daily  20 snapshots: width ratio 0.999 (IQR 0.992-1.005), median shift     +0
    Hourly q 0.00-0.05: n  57, mean gap +0.0010
    Hourly q 0.05-0.15: n  86, mean gap -0.0001
    Hourly q 0.15-0.35: n  91, mean gap -0.0005
    Hourly q 0.35-0.65: n 107, mean gap -0.0005
    Hourly q 0.65-0.85: n  92, mean gap -0.0004
    Hourly q 0.85-0.95: n  83, mean gap +0.0000
    Hourly q 0.95-1.00: n  80, mean gap -0.0013
    Daily  q 0.00-0.05: n   1, mean gap -0.0014
    Daily  q 0.05-0.15: n 111, mean gap -0.0002
    Daily  q 0.15-0.35: n 135, mean gap +0.0004
    Daily  q 0.35-0.65: n 161, mean gap +0.0000
    Daily  q 0.65-0.85: n 143, mean gap -0.0002
    Daily  q 0.85-0.95: n 108, mean gap -0.0002
    Daily  q 0.95-1.00: n   1, mean gap +0.0019
    no actionable quotes
    |}]
;;

let%expect_test "planted: Kalshi's distribution 30% wider" =
  (* The truth is the smile with every vol x1.3. With a skew that makes
     the quartile range about 1.28x wider rather than exactly 1.3x (see
     test_models), and each snapshot's skew differs a little, so the base
     smile's exact ratio is printed next to what the analysis recovers
     from cent-rounded quotes. The gap signature of a
     wider distribution: Kalshi above the model where the model is
     unlikely, below it where the model is likely. *)
  let factor = 1.3 in
  let rows = run ~kalshi:(Model_choice.model (Scaled_smile factor)) () in
  let exact =
    let width s = Smile.quantile_above s 0.25 -. Smile.quantile_above s 0.75 in
    let s = Synthetic.smile () in
    width (Smile.scale_vol s ~factor) /. width s
  in
  printf "exact width ratio on the base smile: %.3f\n" exact;
  print_shapes rows;
  print_buckets rows;
  [%expect {|
    exact width ratio on the base smile: 1.277
    Hourly 20 snapshots: width ratio 1.292 (IQR 1.284-1.303), median shift     +1
    Daily  20 snapshots: width ratio 1.278 (IQR 1.269-1.286), median shift    +16
    Hourly q 0.00-0.05: n  81, mean gap +0.0400
    Hourly q 0.05-0.15: n  86, mean gap +0.0587
    Hourly q 0.15-0.35: n  91, mean gap +0.0513
    Hourly q 0.35-0.65: n 107, mean gap +0.0004
    Hourly q 0.65-0.85: n  92, mean gap -0.0498
    Hourly q 0.85-0.95: n  83, mean gap -0.0584
    Hourly q 0.95-1.00: n  80, mean gap -0.0405
    Daily  q 0.00-0.05: n   1, mean gap +0.0486
    Daily  q 0.05-0.15: n 111, mean gap +0.0568
    Daily  q 0.15-0.35: n 135, mean gap +0.0488
    Daily  q 0.35-0.65: n 161, mean gap +0.0022
    Daily  q 0.65-0.85: n 143, mean gap -0.0457
    Daily  q 0.85-0.95: n 108, mean gap -0.0545
    Daily  q 0.95-1.00: n   1, mean gap -0.0481
    |}]
;;

let%expect_test "planted: Kalshi centred $300 higher" =
  (* The truth is the same smile around a forward $300 higher. *)
  let shifted smile =
    Smile.create ~spot:(Smile.spot smile) ~forward:(Smile.forward smile +. 300.)
      ~years:(Smile.years smile) ~k_min:(-0.15) ~k_max:0.15 (Smile.params smile)
    |> Or_error.ok_exn
    |> Replication.Model.of_smile
  in
  print_shapes (run ~kalshi:shifted ());
  [%expect {|
    Hourly 20 snapshots: width ratio 1.002 (IQR 0.998-1.006), median shift   +298
    Daily  20 snapshots: width ratio 1.005 (IQR 0.999-1.012), median shift   +301
    |}]
;;

let%expect_test "the edge detector fires when there is edge to find" =
  (* A much wider truth and a tight half-cent spread. Where the model says
     a strike is unlikely, Kalshi's YES is now rich enough that selling it
     beats the fee; where the model says likely, YES is cheap enough to
     buy. The 5%-95% filter keeps the extreme tails out. Under the null
     test above the same detector finds nothing. *)
  print_actionable
    (run ~half_spread:0.005 ~kalshi:(Model_choice.model (Scaled_smile 1.6)) ());
  [%expect {|
    Hourly buy YES : 203 quotes, mean edge +0.0628
    Hourly sell YES: 209 quotes, mean edge +0.0640
    Daily  buy YES : 283 quotes, mean edge +0.0553
    Daily  sell YES: 289 quotes, mean edge +0.0587
    |}]
;;
