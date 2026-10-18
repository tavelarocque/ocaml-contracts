(* Greeks of Kalshi "above" contracts, and the cost of hedging one, from
   Implied Odds' smiles.

     dune exec bin/greeks.exe -- ~/CODE/implied-odds/data/export/ocaml

   Reads only surfaces.csv: these are option-implied sensitivities, with
   no Kalshi quotes involved. For each event in the latest snapshot it
   prints, at strikes around the forward, each contract's price and how it
   moves: delta, scaled to a $100 move in BTC, under each rule for how the
   smile moves,
   and per 1% rise in every vol. Then it simulates selling the
   at-the-money contract and delta-hedging it on a 200-step implied tree. *)

open! Core
open Contracts

let nice_step sd =
  List.find [ 50.; 100.; 250.; 500.; 1000.; 2000.; 5000. ] ~f:(fun s -> Float.(s >= sd /. 2.))
  |> Option.value ~default:10_000.
;;

let () =
  let dir =
    match Sys.get_argv () with
    | [| _; dir |] -> dir
    | _ ->
      eprintf "usage: greeks.exe EXPORT_DIR\n";
      exit 2
  in
  let surfaces = Export.load_surfaces (dir ^/ "surfaces.csv") |> Or_error.ok_exn in
  let latest =
    List.map surfaces ~f:(fun s -> s.snap_utc)
    |> List.max_elt ~compare:String.compare
    |> Option.value_exn
  in
  printf "snapshot %s. Prices and moves in cents per $1 contract.\n" latest;
  List.filter surfaces ~f:(fun s -> String.equal s.snap_utc latest)
  |> List.sort ~compare:(fun (a : Export.Surface.t) b -> String.compare a.event b.event)
  |> List.iter ~f:(fun (s : Export.Surface.t) ->
    let smile = s.smile in
    let expiry = Smile.years smile
    and forward = Smile.forward smile in
    let sd = forward *. Smile.atm_vol smile *. Float.sqrt expiry in
    let step = nice_step sd in
    printf "\n%s (%s, %.1f hours to close), forward %.0f\n" s.event
      (Sexp.to_string [%sexp (s.kind : Export.Kind.t)])
      s.hours_to_close forward;
    Option.iter (Smile.kink smile) ~f:(fun k ->
      printf "  Note: near-sharp kink at %.0f; Greeks near it reflect the fit's shape.\n" k);
    printf "  %8s %7s %22s %19s %12s\n" "above" "price" "per +$100 (moneyness)"
      "per +$100 (strike)" "per +1% vol";
    List.iter (Kalshi.edges_around ~centre:forward ~step ~each_side:3) ~f:(fun strike ->
      let c = Kalshi.above ~expiry ~strike in
      let g sticky = Greeks.compute_exn ~sticky Smile smile c in
      let m = g `Moneyness
      and k = g `Strike in
      printf "  %8.0f %7.2f %22.2f %19.2f %12.2f\n" strike (100. *. m.value)
        (100. *. 100. *. m.delta) (100. *. 100. *. k.delta) (100. *. m.vega));
    let tree =
      Lattice.create ~steps:200 (Model_choice.model Smile smile) |> Or_error.ok_exn
    in
    let atm = Float.round_nearest (forward /. step) *. step in
    printf "  Sell \"above %.0f\" and delta-hedge (200-step tree, %.1f minutes a step):\n" atm
      (Lattice.dt tree *. 365. *. 24. *. 60.);
    List.iter [ 1; 4; 16 ] ~f:(fun every ->
      match
        Hedge.simulate tree (Kalshi.above ~expiry ~strike:atm) ~rebalance_every:every
          ~paths:4000 ~seed:1
      with
      | Error e -> printf "    unpriceable: %s\n" (Sexp.to_string [%sexp (e : Unpriceable.t)])
      | Ok paths ->
        let mean, sd = Hedge.mean_sd paths in
        printf "    re-hedge every %2d steps: P&L mean %+5.2f, sd %5.2f\n" every (100. *. mean)
          (100. *. sd)))
;;
