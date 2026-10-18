(* Selling a contract and delta-hedging it on the implied tree. Synthetic
   smile only. *)

open! Core
open Contracts

let smile = Synthetic.smile ()
let expiry = Smile.years smile
let forward = Smile.forward smile

let tree =
  lazy (Lattice.create ~steps:200 (Model_choice.model Smile smile) |> Or_error.ok_exn)
;;

let sold c ~every = Hedge.simulate (force tree) c ~rebalance_every:every ~paths:4000 ~seed:1
  |> Result.map_error ~f:(fun e -> Error.create_s [%sexp (e : Unpriceable.t)])
  |> Or_error.ok_exn
let digital = Kalshi.above ~expiry ~strike:forward
let call = Instruments.call ~expiry ~strike:forward

let%expect_test "hedging every step replicates exactly" =
  (* Two outcomes per step, two instruments: the tree is a complete market,
     so a seller who re-hedges at every step ends with nothing, on every
     path, whatever the contract. *)
  List.iter [ "digital", digital; "call", call ] ~f:(fun (name, c) ->
    let worst =
      Array.fold (sold c ~every:1) ~init:0. ~f:(fun acc p -> Float.max acc (Float.abs p.pnl))
    in
    (* The exact size is rounding error, about 7e-14 for the digital and 3e-10 for the
       call (its P&L is in dollars of BTC price, not cents), and differs in the last digit between platforms' maths
       libraries, so the test prints a bound rather than the number. *)
    let bound = if String.equal name "digital" then 1e-12 else 1e-8 in
    printf "%-8s worst |P&L| over 4000 paths below %.0e: %b\n" name bound Float.(worst < bound));
  [%expect {|
    digital  worst |P&L| over 4000 paths below 1e-12: true
    call     worst |P&L| over 4000 paths below 1e-08: true
    |}]
;;

let%expect_test "hedging less often leaves risk: smoothly for a call, not for a digital" =
  (* The tree has 200 steps over one day, about 7 minutes each. Hedging
     every k steps, the seller's P&L still averages zero (the premium was
     fair, and the means are within their standard errors), but now it has
     a spread. Figures are cents per contract for the digital and dollars
     per BTC for the call.

     The call behaves as textbooks say: between re-hedges its value curves
     away from the straight-line hedge (gamma), and the error grows like
     sqrt(k). Quadrupling the gap doubles the sd: 118, 245, 471.

     The digital doesn't. Its sd is already 13 cents with 4-step gaps and
     only reaches 23 at 64. Its risk isn't the steady curvature of a call.
     It's the jump at the strike in the last minutes: whether the final
     price lands just above or just below the strike, the hedge's last
     adjustment is too late either way. A wider gap adds little to a risk
     that a short gap already fails to catch. *)
  printf "%-12s %22s %22s\n" "re-hedge" "digital, cents" "call, dollars";
  List.iter [ 1; 4; 16; 64 ] ~f:(fun every ->
    let show c scale =
      let paths = sold c ~every in
      let mean, sd = Hedge.mean_sd paths in
      let se = sd /. Float.sqrt (Float.of_int (Array.length paths)) in
      sprintf "%+6.2f (+-%.2f) sd %5.2f" (scale *. mean) (scale *. se) (scale *. sd)
    in
    printf "%3d steps    %22s %22s\n" every (show digital 100.) (show call 1.));
  [%expect {|
    re-hedge             digital, cents          call, dollars
      1 steps     +0.00 (+-0.00) sd  0.00  -0.00 (+-0.00) sd  0.00
      4 steps     -0.04 (+-0.21) sd 13.28  +0.39 (+-1.87) sd 118.25
     16 steps     -0.10 (+-0.28) sd 17.99  -1.26 (+-3.87) sd 244.91
     64 steps     -0.18 (+-0.37) sd 23.45  +3.58 (+-7.45) sd 470.96
    |}]
;;

let%expect_test "a digital's risk sits at its strike" =
  (* Where the error comes from. Near expiry a digital's value jumps from 0
     to 1 across the strike, so its delta spikes there and a hedge that
     isn't adjusted constantly can't follow it. Paths that finish within
     $500 of the strike carry four times the error of paths that finish
     $2,000 or more away. Those far paths still carry some, because they
     may have passed near the strike late in the day. Traders call this
     pin risk, and it is why a market maker widens its quotes on contracts
     struck near the price as the close approaches. *)
  let paths = sold digital ~every:16 in
  let band lo hi =
    Array.filter paths ~f:(fun p ->
      let d = Float.abs (p.terminal -. forward) in
      Float.(d >= lo && d < hi))
  in
  List.iter [ 0., 500.; 500., 2000.; 2000., Float.infinity ] ~f:(fun (lo, hi) ->
    let ps = band lo hi in
    let _, sd = Hedge.mean_sd ps in
    printf "finish %5.0f to %5.0f from the strike: %4d paths, P&L sd %5.2f cents\n" lo hi
      (Array.length ps) (100. *. sd));
  [%expect {|
    finish     0 to   500 from the strike:  661 paths, P&L sd 34.54 cents
    finish   500 to  2000 from the strike: 1571 paths, P&L sd 15.16 cents
    finish  2000 to   inf from the strike: 1768 paths, P&L sd  8.33 cents
    |}]
;;
