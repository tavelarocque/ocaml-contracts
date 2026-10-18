(* The quoting rule, and the market maker in a simulated market. Synthetic
   smile and synthetic order flow only. *)

open! Core
open Contracts

let base =
  { Market_maker.Params.gamma = 0.01; k = 50.; alpha = 0.; max_inventory = 50; hedge = false }
;;

let show_quote = Option.value_map ~default:"  --" ~f:(fun x -> sprintf "%4.0f" (100. *. x))

let%expect_test "quotes lean against inventory and drop a side at the limit" =
  (* Fair value 50 cents. The flow term ln(1 + gamma/k)/gamma is about
     1/k = 2 cents, the inventory term gamma p(1-p)/2 is negligible, so the
     half-spread is about 2 cents before rounding outward to whole cents.
     Each contract held moves the reservation price by gamma p(1-p) =
     0.25 cents: long 20, the quotes sit 5 cents lower. *)
  printf "%10s %5s %5s\n" "inventory" "bid" "ask";
  List.iter [ -50; -20; 0; 20; 50 ] ~f:(fun inventory ->
    let bid, ask = Market_maker.quotes base ~fair:0.5 ~step_sd:0. ~inventory in
    printf "%10d %5s %5s\n" inventory (show_quote bid) (show_quote ask));
  [%expect {|
    inventory   bid   ask
          -50    60    --
          -20    52    58
            0    47    53
           20    42    48
           50    --    40
    |}]
;;

let%expect_test "quotes near 0 and 1, and the adverse-selection term" =
  (* Near the edges, a side that would fall outside 1-99 cents is dropped.
     With alpha = 2, the spread widens by two standard deviations of the
     fair value's next move. *)
  List.iter [ 0.02, 0.; 0.97, 0.; 0.5, 0.01; 0.5, 0.03 ] ~f:(fun (fair, step_sd) ->
    let bid, ask =
      Market_maker.quotes { base with alpha = 2. } ~fair ~step_sd ~inventory:0
    in
    printf "fair %4.0f, next-move sd %4.1f: bid %s ask %s\n" (100. *. fair)
      (100. *. step_sd) (show_quote bid) (show_quote ask));
  [%expect {|
    fair    2, next-move sd  0.0: bid   -- ask    5
    fair   97, next-move sd  0.0: bid   94 ask   --
    fair   50, next-move sd  1.0: bid   45 ask   55
    fair   50, next-move sd  3.0: bid   41 ask   59
    |}]
;;

let smile = Synthetic.smile ()
let expiry = Smile.years smile
let atm = Kalshi.above ~expiry ~strike:(Smile.forward smile)

let tree =
  lazy (Lattice.create ~steps:200 (Model_choice.model Smile smile) |> Or_error.ok_exn)
;;

let noise_only = { Market_maker.Flow.noise_rate = 0.3; informed_rate = 0.; lookahead = 1 }

let run ?(paths = 2000) params flow =
  Market_maker.simulate (force tree) atm params flow ~paths ~seed:3
  |> Result.map_error ~f:(fun e -> Error.create_s [%sexp (e : Unpriceable.t)])
  |> Or_error.ok_exn
;;

let mean xs ~f =
  Array.sum (module Float) xs ~f /. Float.of_int (Array.length xs)
;;

let sd xs ~f =
  let m = mean xs ~f in
  Float.sqrt (Array.sum (module Float) xs ~f:(fun x -> Float.square (f x -. m))
              /. Float.of_int (Array.length xs - 1))
;;

let summary label outcomes =
  let open Market_maker in
  let n = Float.of_int (Array.length outcomes) in
  printf "%-20s P&L %+5.2f (+-%.2f) sd %4.2f | fills %4.1f + %4.1f informed\n" label
    (mean outcomes ~f:(fun o -> o.pnl))
    (sd outcomes ~f:(fun o -> o.pnl) /. Float.sqrt n)
    (sd outcomes ~f:(fun o -> o.pnl))
    (mean outcomes ~f:(fun o -> Float.of_int o.noise_fills))
    (mean outcomes ~f:(fun o -> Float.of_int o.informed_fills))
;;

(* Dollars per path below are for a market maker quoting one-contract
   orders in the at-the-money "above" contract for a day, on a 200-step
   tree (about 7 minutes a step). *)

let%expect_test "with uninformed flow, P&L averages exactly the edge captured" =
  (* The fair value is a martingale, so inventory gains and losses average
     zero against traders who know nothing: what's left on average is the
     spread captured at each fill, measured against fair value at the
     time. The printed gap, P&L minus edge, is zero within its standard
     error, unhedged and hedged. *)
  List.iter [ "unhedged", base; "hedged", { base with hedge = true } ] ~f:(fun (label, params) ->
    let o = run params noise_only in
    let gap = fun (o : Market_maker.outcome) -> o.pnl -. o.edge in
    printf "%-9s edge %.3f, P&L %.3f, P&L - edge %+.3f (+-%.3f)\n" label
      (mean o ~f:(fun o -> o.edge)) (mean o ~f:(fun o -> o.pnl)) (mean o ~f:gap)
      (sd o ~f:gap /. Float.sqrt (Float.of_int (Array.length o))));
  [%expect {|
    unhedged  edge 0.758, P&L 0.738, P&L - edge -0.020 (+-0.024)
    hedged    edge 0.758, P&L 0.756, P&L - edge -0.002 (+-0.004)
    |}]
;;

let%expect_test "informed traders take the difference" =
  (* Traders who see the fair value one step (7 minutes) ahead trade only
     when it is about to cross the quote. The edge at the moment of the
     fill still looks positive; the loss shows up in the next move. Their
     cost, edge minus P&L, grows with how often they come. *)
  List.iter [ 0.; 0.1; 0.2; 0.4 ] ~f:(fun informed_rate ->
    let o = run base { noise_only with informed_rate } in
    printf "informed %.1f a step: edge %.2f, P&L %+.2f, cost of informed flow %.2f\n"
      informed_rate (mean o ~f:(fun o -> o.edge)) (mean o ~f:(fun o -> o.pnl))
      (mean o ~f:(fun o -> o.edge -. o.pnl)));
  [%expect {|
    informed 0.0 a step: edge 0.76, P&L +0.74, cost of informed flow 0.02
    informed 0.1 a step: edge 1.04, P&L +0.62, cost of informed flow 0.42
    informed 0.2 a step: edge 1.33, P&L +0.49, cost of informed flow 0.84
    informed 0.4 a step: edge 1.88, P&L +0.18, cost of informed flow 1.70
    |}]
;;

let%expect_test "inventory risk: leaning the quotes, and hedging" =
  (* With gamma = 0 the quotes ignore inventory, which drifts like a random
     walk, and the P&L's spread is large. Raising gamma leans the quotes
     to unload inventory: much less risk, a little less profit. Hedging
     the inventory in the forward removes most of the rest; what stays is
     the one step each new fill rides unhedged, and the edge's own noise. *)
  List.iter [ 0.; 0.01; 0.05; 0.2 ] ~f:(fun gamma ->
    summary (sprintf "gamma %.2f" gamma) (run { base with gamma } noise_only);
    summary (sprintf "gamma %.2f, hedged" gamma) (run { base with gamma; hedge = true } noise_only));
  [%expect {|
    gamma 0.00           P&L +0.84 (+-0.05) sd 2.26 | fills 31.8 +  0.0 informed
    gamma 0.00, hedged   P&L +0.78 (+-0.01) sd 0.25 | fills 31.8 +  0.0 informed
    gamma 0.01           P&L +0.74 (+-0.02) sd 1.08 | fills 30.6 +  0.0 informed
    gamma 0.01, hedged   P&L +0.76 (+-0.01) sd 0.24 | fills 30.6 +  0.0 informed
    gamma 0.05           P&L +0.69 (+-0.01) sd 0.51 | fills 28.3 +  0.0 informed
    gamma 0.05, hedged   P&L +0.71 (+-0.01) sd 0.24 | fills 28.3 +  0.0 informed
    gamma 0.20           P&L +0.51 (+-0.01) sd 0.26 | fills 20.2 +  0.0 informed
    gamma 0.20, hedged   P&L +0.50 (+-0.00) sd 0.20 | fills 20.2 +  0.0 informed
    |}]
;;

let%expect_test "widening for jumps pays against a short look ahead, not a long one" =
  (* Informed traders arrive 0.3 times a step and see the fair value 1, 4
     or 10 steps ahead. alpha widens the spread by that many standard
     deviations of the fair value's next one-step move.

     Against a 1-step look ahead, half a standard deviation is the best of
     these: it turns away most informed orders and keeps half the
     uninformed ones. Against 4 steps the loss shrinks as alpha grows but
     never turns positive here. Against 10 steps every spread tried loses
     more than a dollar a day or close to it: widening turns away the
     uninformed faster than the informed, whose information is worth more
     than any of these spreads. Not quoting at all, P&L 0, beats every row.
     That's Glosten and Milgrom's result: with enough informed flow the
     market breaks down. One more lesson in the table: alpha is scaled to
     the one-step move, but a trader who sees h steps ahead knows a move
     about sqrt(h) times larger, so the right alpha depends on who you
     think you're trading with. *)
  List.iter [ 1; 4; 10 ] ~f:(fun lookahead ->
    List.iter [ 0.; 0.5; 1.; 2. ] ~f:(fun alpha ->
      summary
        (sprintf "look %2d, alpha %.1f" lookahead alpha)
        (run { base with alpha } { noise_only with informed_rate = 0.3; lookahead })));
  [%expect {|
    look  1, alpha 0.0   P&L +0.32 (+-0.03) sd 1.28 | fills 30.9 + 33.7 informed
    look  1, alpha 0.5   P&L +0.54 (+-0.02) sd 0.97 | fills 15.7 +  4.6 informed
    look  1, alpha 1.0   P&L +0.41 (+-0.02) sd 0.84 | fills  9.0 +  0.0 informed
    look  1, alpha 2.0   P&L +0.19 (+-0.01) sd 0.52 | fills  4.3 +  0.0 informed
    look  4, alpha 0.0   P&L -0.87 (+-0.03) sd 1.44 | fills 31.7 + 30.7 informed
    look  4, alpha 0.5   P&L -0.50 (+-0.04) sd 1.68 | fills 16.1 + 28.6 informed
    look  4, alpha 1.0   P&L -0.27 (+-0.03) sd 1.50 | fills  9.1 + 22.1 informed
    look  4, alpha 2.0   P&L -0.06 (+-0.02) sd 1.10 | fills  4.3 +  5.9 informed
    look 10, alpha 0.0   P&L -2.04 (+-0.04) sd 1.68 | fills 32.3 + 37.7 informed
    look 10, alpha 0.5   P&L -1.65 (+-0.04) sd 1.86 | fills 16.6 + 35.4 informed
    look 10, alpha 1.0   P&L -1.41 (+-0.04) sd 1.76 | fills  9.4 + 29.8 informed
    look 10, alpha 2.0   P&L -0.96 (+-0.04) sd 1.58 | fills  4.4 + 16.7 informed
    |}]
;;
