(* "Will BTC touch $X before the close?" priced from Implied Odds' smiles.

     dune exec bin/touch.exe -- ~/CODE/implied-odds/data/export/ocaml [STEPS]

   For each Kalshi event in the latest snapshot, builds an implied binomial
   tree (default 400 steps) from that event's smile and prints, at Kalshi's
   strikes, the chance of finishing past the strike at the close next to
   the chance of touching it at any point before. The tree checks the
   barrier only at its steps, so with fewer steps "touch" reads a little low. *)

open! Core
open Contracts

let () =
  let dir, steps =
    match Sys.get_argv () with
    | [| _; dir |] -> dir, 400
    | [| _; dir; steps |] -> dir, Int.of_string steps
    | _ ->
      eprintf "usage: touch.exe EXPORT_DIR [STEPS]\n";
      exit 2
  in
  let pairs =
    (let open Or_error.Let_syntax in
     let%bind surfaces = Export.load_surfaces (dir ^/ "surfaces.csv")
     and rows = Export.load_rows (dir ^/ "contracts.csv") in
     Export.join surfaces rows)
    |> Or_error.ok_exn
  in
  let latest =
    List.map pairs ~f:(fun (s, _) -> s.snap_utc) |> List.max_elt ~compare:String.compare
    |> Option.value_exn
  in
  printf "snapshot %s, %d-step trees\n" latest steps;
  let events =
    List.filter_map pairs ~f:(fun ((s : Export.Surface.t), _) ->
      Option.some_if (String.equal s.snap_utc latest) s)
    |> List.dedup_and_sort ~compare:(fun (a : Export.Surface.t) b -> String.compare a.event b.event)
  in
  List.iter events ~f:(fun s ->
    let smile = s.smile in
    let expiry = Smile.years smile in
    let forward = Smile.forward smile in
    let tree = Lattice.create ~steps (Replication.Model.of_smile smile) |> Or_error.ok_exn in
    let p c = Lattice.price_exn tree c in
    printf "\n%s (%s, %.1f hours to close), forward %.0f\n" s.event
      (Sexp.to_string [%sexp (s.kind : Export.Kind.t)])
      s.hours_to_close forward;
    Option.iter (Smile.kink smile) ~f:(fun price ->
      printf "  Note: this smile has a near-sharp kink at %.0f (SVI s = %.1e).\n" price
        (Smile.params smile).s;
      printf "  Probabilities near that price come from the fitted shape more than the market.\n");
    printf "  %10s  %-20s %8s %8s %6s\n" "strike" "" "at close" "touch" "ratio";
    List.filter_map pairs ~f:(fun ((s' : Export.Surface.t), (r : Export.Row.t)) ->
      Option.some_if
        (String.equal s'.snap_utc latest && String.equal s'.event s.event && r.in_range)
        r.strike)
    |> List.sort ~compare:Float.compare
    |> List.iter ~f:(fun k ->
      let above = p (Instruments.digital_above ~expiry ~strike:k) in
      let line label close touch =
        if Float.(close >= 0.01 && close <= 0.6)
        then printf "  %10.2f  %-20s %8.3f %8.3f %6.2f\n" k label close touch (touch /. close)
      in
      if Float.(k > forward)
      then line "rises to or above" above (p (Instruments.one_touch_above ~expiry ~barrier:k))
      else line "falls to or below" (1. -. above) (p (Instruments.one_touch_below ~expiry ~barrier:k))))
;;
