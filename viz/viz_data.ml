(* The numbers behind the project's charts, written as CSV files.

     dune exec viz/viz_data.exe -- OUT_DIR

   Every figure is computed here by the library itself, from the same
   synthetic inputs the tests use (test/synthetic.ml, test/synthetic_hf.ml):
   a one-day BTC smile at a $100,000 forward with a downward skew, and a
   simulated 12-hour market. Nothing reads real data, so nothing here
   touches the sealed comparison of Kalshi's prices with the options. *)

open! Core
open Contracts
module Synthetic = Contracts_test.Synthetic
module Synthetic_hf = Contracts_test.Synthetic_hf

let out = ref "."

let write name header rows =
  let path = !out ^/ name in
  Out_channel.with_file path ~f:(fun oc ->
    Out_channel.output_string oc (String.concat ~sep:"," header ^ "\n");
    List.iter rows ~f:(fun r -> Out_channel.output_string oc (String.concat ~sep:"," r ^ "\n")));
  eprintf "wrote %s (%d rows)\n%!" path (List.length rows)
;;

let f6 = sprintf "%.6g"
let f4 = sprintf "%.4f"
let smile = Synthetic.smile ()
let expiry = Smile.years smile
let forward = Smile.forward smile
let models = Model_choice.defaults
let price m c = Replication.price_exn m c

(* 1. The distribution: implied vol, the probability of finishing above
   each strike, and the density (probability per $1,000), under the smile,
   a lognormal at the smile's at-the-money vol, and the smile narrowed to
   80%. *)
let distribution () =
  let strikes = List.init 161 ~f:(fun i -> 92_000. +. (100. *. Float.of_int i)) in
  let rows =
    List.concat_map models ~f:(fun choice ->
      let m = Model_choice.model choice smile in
      let s =
        match choice with
        | Smile -> smile
        | Lognormal_atm -> Smile.flat smile ~sigma:(Smile.atm_vol smile)
        | Scaled_smile f -> Smile.scale_vol smile ~factor:f
      in
      List.map strikes ~f:(fun k ->
        let h = 25. in
        let p = Replication.Model.prob_above m k in
        let dens =
          (Replication.Model.prob_above m (k -. h) -. Replication.Model.prob_above m (k +. h))
          /. (2. *. h) *. 1000.
        in
        [ Model_choice.name choice; sprintf "%.0f" k; f6 (100. *. Smile.sigma s k); f6 p; f6 dens ]))
  in
  write "distribution.csv" [ "model"; "strike"; "iv_pct"; "p_above"; "density_per_1000" ] rows
;;

(* 2. A Kalshi ladder of $1,000 buckets, priced under each model. *)
let ladder () =
  let edges = Kalshi.edges_around ~centre:forward ~step:1000. ~each_side:5 in
  let rows =
    List.concat_map models ~f:(fun choice ->
      let m = Model_choice.model choice smile in
      List.mapi (Kalshi.ladder ~expiry ~edges) ~f:(fun i (label, c) ->
        [ Model_choice.name choice; Int.to_string i; label; f6 (100. *. price m c) ]))
  in
  write "ladder.csv" [ "model"; "order"; "bucket"; "price_cents" ] rows
;;

(* 3. Greeks of "BTC above K" across strikes, under both rules for how the
   smile moves with the price. Delta per $100 move, in cents; vega per 1%
   of vol, in cents. The density per $100 is printed alongside: under
   sticky moneyness a digital's delta is close to it. *)
let greeks () =
  let m = Model_choice.model Smile smile in
  let rows =
    List.concat_map [ "moneyness", `Moneyness; "strike", `Strike ] ~f:(fun (name, sticky) ->
      List.map (List.init 41 ~f:(fun i -> 96_000. +. (200. *. Float.of_int i))) ~f:(fun k ->
        let g = Greeks.compute_exn ~sticky Smile smile (Kalshi.above ~expiry ~strike:k) in
        let h = 25. in
        let dens =
          (Replication.Model.prob_above m (k -. h) -. Replication.Model.prob_above m (k +. h))
          /. (2. *. h)
        in
        [ name
        ; sprintf "%.0f" k
        ; f6 (100. *. g.value)
        ; f6 (100. *. 100. *. g.delta)
        ; f6 (100. *. 100. *. dens)
        ; f6 (100. *. g.vega)
        ]))
  in
  write "greeks.csv"
    [ "sticky"; "strike"; "value_cents"; "delta_cents_per_100"; "density_cents_per_100"; "vega_cents" ]
    rows
;;

let tree = lazy (Lattice.create ~steps:200 (Model_choice.model Smile smile) |> Or_error.ok_exn)

let sold c ~every =
  Hedge.simulate (force tree) c ~rebalance_every:every ~paths:4000 ~seed:1
  |> Result.map_error ~f:(fun e -> Error.create_s [%sexp (e : Unpriceable.t)])
  |> Or_error.ok_exn
;;

(* 4. Selling and delta-hedging: each path's P&L against where BTC finished
   (the at-the-money digital, re-hedged every 16 steps of 200), and the
   P&L's spread as re-hedging gets rarer, for the digital and a call. *)
let hedging () =
  let digital = Kalshi.above ~expiry ~strike:forward
  and call = Instruments.call ~expiry ~strike:forward in
  write "hedge_paths.csv" [ "path"; "terminal"; "pnl_cents" ]
    (Array.to_list (sold digital ~every:16)
     |> List.mapi ~f:(fun i (p : Hedge.path) ->
       [ Int.to_string i; sprintf "%.1f" p.terminal; sprintf "%.3f" (100. *. p.pnl) ]));
  write "hedge_frequency.csv" [ "contract"; "rebalance_every"; "minutes"; "pnl_sd"; "unit" ]
    (List.concat_map [ 1; 4; 16; 64 ] ~f:(fun every ->
       let minutes = Float.of_int every *. 1440. /. 200. in
       List.map [ "digital", digital, 100., "cents"; "call", call, 1., "dollars" ]
         ~f:(fun (name, c, scale, unit) ->
           let _, sd = Hedge.mean_sd (sold c ~every) in
           [ name; Int.to_string every; sprintf "%.1f" minutes; f4 (scale *. sd); unit ])))
;;

(* 5. The market maker: its quotes against inventory, and three simulated
   experiments on the at-the-money digital (200 steps over a day, 2,000
   paths each), as in test/test_market_maker.ml. *)
let base =
  { Market_maker.Params.gamma = 0.01; k = 50.; alpha = 0.; max_inventory = 50; hedge = false }
;;

let noise = { Market_maker.Flow.noise_rate = 0.3; informed_rate = 0.; lookahead = 1 }

let run params flow =
  Market_maker.simulate (force tree) (Kalshi.above ~expiry ~strike:forward) params flow
    ~paths:2000 ~seed:3
  |> Result.map_error ~f:(fun e -> Error.create_s [%sexp (e : Unpriceable.t)])
  |> Or_error.ok_exn
;;

let mean xs ~f = Array.sum (module Float) xs ~f /. Float.of_int (Array.length xs)

let sd xs ~f =
  let m = mean xs ~f in
  Float.sqrt
    (Array.sum (module Float) xs ~f:(fun x -> Float.square (f x -. m))
     /. Float.of_int (Array.length xs - 1))
;;

let market_making () =
  let cents = Option.value_map ~default:"" ~f:(fun x -> sprintf "%.0f" (100. *. x)) in
  write "quotes.csv" [ "inventory"; "bid_cents"; "ask_cents"; "reservation_cents" ]
    (List.map (List.init 21 ~f:(fun i -> -50 + (5 * i))) ~f:(fun q ->
       let bid, ask = Market_maker.quotes base ~fair:0.5 ~step_sd:0. ~inventory:q in
       (* r = p - q gamma p (1 - p), the centre the quotes lean around *)
       let r = 0.5 -. (Float.of_int q *. base.gamma *. 0.25) in
       [ Int.to_string q; cents bid; cents ask; sprintf "%.2f" (100. *. r) ]));
  write "mm_informed.csv" [ "informed_per_step"; "edge"; "pnl"; "cost" ]
    (List.map [ 0.; 0.1; 0.2; 0.3; 0.4 ] ~f:(fun informed_rate ->
       let o = run base { noise with informed_rate } in
       let e = mean o ~f:(fun o -> o.edge)
       and p = mean o ~f:(fun o -> o.pnl) in
       [ sprintf "%.1f" informed_rate; f4 e; f4 p; f4 (e -. p) ]));
  write "mm_inventory.csv" [ "gamma"; "hedged"; "pnl_mean"; "pnl_sd"; "fills" ]
    (List.concat_map [ 0.; 0.01; 0.05; 0.2 ] ~f:(fun gamma ->
       List.map [ false; true ] ~f:(fun hedge ->
         let o = run { base with gamma; hedge } noise in
         [ sprintf "%.2f" gamma
         ; (if hedge then "hedged" else "unhedged")
         ; f4 (mean o ~f:(fun o -> o.pnl))
         ; f4 (sd o ~f:(fun o -> o.pnl))
         ; sprintf "%.1f" (mean o ~f:(fun o -> Float.of_int o.noise_fills))
         ])));
  write "mm_alpha.csv" [ "lookahead_steps"; "alpha"; "pnl_mean"; "pnl_se"; "informed_fills" ]
    (List.concat_map [ 1; 4; 10 ] ~f:(fun lookahead ->
       List.map [ 0.; 0.5; 1.; 1.5; 2. ] ~f:(fun alpha ->
         let o = run { base with alpha } { noise with informed_rate = 0.3; lookahead } in
         [ Int.to_string lookahead
         ; sprintf "%.1f" alpha
         ; f4 (mean o ~f:(fun o -> o.pnl))
         ; f4 (sd o ~f:(fun o -> o.pnl) /. Float.sqrt (Float.of_int (Array.length o)))
         ; sprintf "%.1f" (mean o ~f:(fun o -> Float.of_int o.informed_fills))
         ])))
;;

(* 6. Markouts on the synthetic 12-hour market: the passive side's
   volume-weighted markout at each horizon, with its 95% interval from
   resampling whole events, as the share of informed takers rises. *)
let markouts () =
  let index = Synthetic_hf.index_path ~seed:5 in
  let fair =
    Markout.Fair.create ~surfaces:(Synthetic_hf.surfaces index) ~index () |> Or_error.ok_exn
  in
  let horizons = [ 0.; 10.; 30.; 60.; 120.; 300. ] in
  write "markouts.csv"
    [ "informed_share"; "horizon"; "seconds"; "trades"; "mean_cents"; "ci_low_cents"; "ci_high_cents" ]
    (List.concat_map [ 0.; 0.5; 0.9 ] ~f:(fun informed ->
       let ms =
         Markout.compute fair ~books:Synthetic_hf.books
           ~trades:(Synthetic_hf.trades index ~informed ~lookahead:60. ~seed:9)
           ~horizons
       in
       List.filter_map (Markout.Summary.compute ~horizons ms) ~f:(fun (r : Markout.Summary.row) ->
         Option.map (Float.of_string_opt (String.chop_suffix_if_exists r.label ~suffix:"s"))
           ~f:(fun secs ->
             [ sprintf "%.0f%%" (100. *. informed)
             ; r.label
             ; sprintf "%.0f" secs
             ; Int.to_string r.trades
             ; f4 (100. *. r.mean)
             ; f4 (100. *. r.ci_low)
             ; f4 (100. *. r.ci_high)
             ]))))
;;

(* 7. Surfaces, for the 3D views. Each is a grid of one quantity over two
   inputs, from the same synthetic inputs as above. *)
let surfaces () =
  (* "BTC above K" as the close approaches: the smile kept at each
     log-moneyness and shortened to the hours left (Smile.with_years, as
     Markout does between snapshots). Price, and delta under sticky
     moneyness. *)
  write "surf_digital.csv" [ "hours"; "strike"; "p_above"; "delta_cents_per_100" ]
    (List.concat_map (List.init 24 ~f:(fun i -> Float.of_int (i + 1))) ~f:(fun hours ->
       let s = Smile.with_years smile ~years:(hours /. 8760.) in
       let m = Model_choice.model Smile s in
       List.map (List.init 41 ~f:(fun i -> 96_000. +. (200. *. Float.of_int i))) ~f:(fun k ->
         let c = Kalshi.above ~expiry:(Smile.years s) ~strike:k in
         let g = Greeks.compute_exn ~sticky:`Moneyness Smile s c in
         [ sprintf "%.0f" hours
         ; sprintf "%.0f" k
         ; f6 (Replication.price_exn m c)
         ; f6 (100. *. 100. *. g.delta)
         ])));
  (* Hedging error of a sold digital by its strike's distance from the
     forward and how often it is re-hedged. *)
  write "surf_hedge.csv" [ "strike_offset"; "rebalance_every"; "minutes"; "pnl_sd_cents" ]
    (List.concat_map (List.init 9 ~f:(fun i -> -2000. +. (500. *. Float.of_int i))) ~f:(fun off ->
       let c = Kalshi.above ~expiry ~strike:(forward +. off) in
       List.map [ 1; 2; 4; 8; 16; 32; 64 ] ~f:(fun every ->
         let _, sd = Hedge.mean_sd (sold c ~every) in
         [ sprintf "%.0f" off
         ; Int.to_string every
         ; sprintf "%.1f" (Float.of_int every *. 1440. /. 200.)
         ; f4 (100. *. sd)
         ])));
  (* Market-maker P&L by the adverse-selection term alpha and how far ahead
     the informed traders (0.3 arrivals a step) see. *)
  write "surf_mm.csv" [ "lookahead_steps"; "alpha"; "pnl_mean" ]
    (List.concat_map (List.init 10 ~f:(fun i -> i + 1)) ~f:(fun lookahead ->
       List.map (List.init 9 ~f:(fun i -> 0.25 *. Float.of_int i)) ~f:(fun alpha ->
         let o = run { base with alpha } { noise with informed_rate = 0.3; lookahead } in
         [ Int.to_string lookahead; sprintf "%.2f" alpha; f4 (mean o ~f:(fun o -> o.pnl)) ])));
  (* Markouts by the share of informed takers (seeing a minute ahead) and
     the horizon. *)
  let index = Synthetic_hf.index_path ~seed:5 in
  let fair =
    Markout.Fair.create ~surfaces:(Synthetic_hf.surfaces index) ~index () |> Or_error.ok_exn
  in
  let horizons = [ 0.; 10.; 20.; 30.; 45.; 60.; 90.; 120.; 180.; 300. ] in
  write "surf_markout.csv" [ "informed_pct"; "seconds"; "mean_cents" ]
    (List.concat_map (List.init 11 ~f:(fun i -> 0.1 *. Float.of_int i)) ~f:(fun informed ->
       let ms =
         Markout.compute fair ~books:Synthetic_hf.books
           ~trades:(Synthetic_hf.trades index ~informed ~lookahead:60. ~seed:9)
           ~horizons
       in
       List.filter_map (Markout.Summary.compute ~resamples:1 ~horizons ms)
         ~f:(fun (r : Markout.Summary.row) ->
           Option.map (Float.of_string_opt (String.chop_suffix_if_exists r.label ~suffix:"s"))
             ~f:(fun secs ->
               [ sprintf "%.0f" (100. *. informed); sprintf "%.0f" secs; f4 (100. *. r.mean) ]))))
;;

let () =
  (match Sys.get_argv () with
   | [| _; dir |] -> out := dir
   | _ -> ());
  distribution ();
  ladder ();
  greeks ();
  hedging ();
  market_making ();
  markouts ();
  surfaces ()
;;
