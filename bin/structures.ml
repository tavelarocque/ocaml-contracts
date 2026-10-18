(* Ranges, butterflies and call-spread digitals priced from Implied Odds'
   smiles, under each choice of distribution.

     dune exec bin/structures.exe -- ~/CODE/implied-odds/data/export/ocaml

   Reads only surfaces.csv: these are option-implied prices of contracts
   built in the language, with no Kalshi quotes involved. For each event
   in the latest snapshot it prints a ladder of ranges around the forward,
   priced under the smile, a lognormal at the smile's at-the-money vol,
   and the smile narrowed to 80% of its vols, then the density at a few
   strikes measured by butterflies. *)

open! Core
open Contracts

(* A round step near half the distribution's standard deviation. *)
let nice_step sd =
  List.find [ 50.; 100.; 250.; 500.; 1000.; 2000.; 5000. ] ~f:(fun s -> Float.(s >= sd /. 2.))
  |> Option.value ~default:10_000.
;;

let () =
  let dir =
    match Sys.get_argv () with
    | [| _; dir |] -> dir
    | _ ->
      eprintf "usage: structures.exe EXPORT_DIR\n";
      exit 2
  in
  let surfaces = Export.load_surfaces (dir ^/ "surfaces.csv") |> Or_error.ok_exn in
  let latest =
    List.map surfaces ~f:(fun s -> s.snap_utc)
    |> List.max_elt ~compare:String.compare
    |> Option.value_exn
  in
  let choices = Model_choice.defaults in
  printf "snapshot %s\n" latest;
  List.filter surfaces ~f:(fun s -> String.equal s.snap_utc latest)
  |> List.sort ~compare:(fun (a : Export.Surface.t) b -> String.compare a.event b.event)
  |> List.iter ~f:(fun (s : Export.Surface.t) ->
    let smile = s.smile in
    let expiry = Smile.years smile
    and forward = Smile.forward smile in
    let sd = forward *. Smile.atm_vol smile *. Float.sqrt expiry in
    let step = nice_step sd in
    let price choice c = Replication.price_exn (Model_choice.model choice smile) c in
    printf "\n%s (%s, %.1f hours to close), forward %.0f, ATM vol %.1f%%\n" s.event
      (Sexp.to_string [%sexp (s.kind : Export.Kind.t)])
      s.hours_to_close forward
      (100. *. Smile.atm_vol smile);
    Option.iter (Smile.kink smile) ~f:(fun k ->
      printf "  Note: near-sharp kink at %.0f; prices near it reflect the fit's shape.\n" k);
    let row label values =
      printf "  %-22s" label;
      List.iter values ~f:(printf " %11.4f");
      printf "\n"
    in
    printf "  %-22s" (sprintf "ranges, $%.0f wide" step);
    List.iter choices ~f:(fun c -> printf " %11s" (Model_choice.name c));
    printf "\n";
    let edges = Kalshi.edges_around ~centre:forward ~step ~each_side:3 in
    let ladder = Kalshi.ladder ~expiry ~edges in
    List.iter ladder ~f:(fun (label, c) -> row label (List.map choices ~f:(fun m -> price m c)));
    row "total" (List.map choices ~f:(fun m ->
      List.sum (module Float) ladder ~f:(fun (_, c) -> price m c)));
    printf "  density per $1,000 (butterfly, half-width $%.0f)\n" (step /. 5.);
    List.iter (List.filteri edges ~f:(fun i _ -> i % 2 = 1)) ~f:(fun strike ->
      row (sprintf "at %.0f" strike)
        (List.map choices ~f:(fun m ->
           1000.
           *. price m (Instruments.density_butterfly ~expiry ~strike ~half_width:(step /. 5.))))))
;;
