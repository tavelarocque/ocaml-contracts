(* The market maker on real smiles, against simulated order flow.

     dune exec bin/market_make.exe -- ~/CODE/implied-odds/data/export/ocaml

   Reads only surfaces.csv. The fair value comes from each event's implied
   tree; the traders are simulated (uninformed arrivals, and informed ones
   who see the fair value a step ahead), so this shows how the quoting rule
   behaves on the shape of a real distribution, not what it would have
   earned on Kalshi. That replay waits for the real quotes, which are sealed
   until 2026-11-30. *)

open! Core
open Contracts

let () =
  let dir =
    match Sys.get_argv () with
    | [| _; dir |] -> dir
    | _ ->
      eprintf "usage: market_make.exe EXPORT_DIR\n";
      exit 2
  in
  let surfaces = Export.load_surfaces (dir ^/ "surfaces.csv") |> Or_error.ok_exn in
  let latest =
    List.map surfaces ~f:(fun s -> s.snap_utc)
    |> List.max_elt ~compare:String.compare
    |> Option.value_exn
  in
  printf "snapshot %s; simulated flow, 2000 paths each; P&L in dollars per path\n" latest;
  let base =
    { Market_maker.Params.gamma = 0.01; k = 50.; alpha = 0.; max_inventory = 50; hedge = false }
  in
  let flow = { Market_maker.Flow.noise_rate = 0.3; informed_rate = 0.; lookahead = 1 } in
  List.filter surfaces ~f:(fun s -> String.equal s.snap_utc latest)
  |> List.sort ~compare:(fun (a : Export.Surface.t) b -> String.compare a.event b.event)
  |> List.iter ~f:(fun (s : Export.Surface.t) ->
    let smile = s.smile in
    let expiry = Smile.years smile
    and forward = Smile.forward smile in
    let tree = Lattice.create ~steps:200 (Model_choice.model Smile smile) |> Or_error.ok_exn in
    let strike = Float.round_nearest (forward /. 100.) *. 100. in
    let contract = Kalshi.above ~expiry ~strike in
    printf "\n%s (%s, %.1f hours to close): quoting \"above %.0f\", fair %.0f cents\n"
      s.event
      (Sexp.to_string [%sexp (s.kind : Export.Kind.t)])
      s.hours_to_close strike
      (100. *. Lattice.price_exn tree contract);
    Option.iter (Smile.kink smile) ~f:(fun k ->
      printf "  Note: near-sharp kink at %.0f; the fair value near it reflects the fit.\n" k);
    let row label params flow =
      match Market_maker.simulate tree contract params flow ~paths:2000 ~seed:3 with
      | Error e -> printf "  %-34s unpriceable: %s\n" label (Sexp.to_string [%sexp (e : Unpriceable.t)])
      | Ok o ->
        let n = Float.of_int (Array.length o) in
        let pnl = Array.map o ~f:(fun o -> o.Market_maker.pnl) in
        let mean = Array.sum (module Float) pnl ~f:Fn.id /. n in
        let sd =
          Float.sqrt (Array.sum (module Float) pnl ~f:(fun x -> Float.square (x -. mean)) /. (n -. 1.))
        in
        printf "  %-34s P&L %+5.2f, sd %4.2f\n" label mean sd
    in
    row "uninformed flow" base flow;
    row "uninformed flow, hedged" { base with hedge = true } flow;
    row "20% informed, plain quotes" base { flow with informed_rate = 0.2 };
    row "20% informed, alpha 0.5" { base with alpha = 0.5 } { flow with informed_rate = 0.2 })
;;
