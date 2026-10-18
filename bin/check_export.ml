(* Check every contract in an Implied Odds export against p_opt.

     dune exec bin/check_export.exe -- ~/CODE/implied-odds/data/export/ocaml

   Each Kalshi contract is written in the contract language
   (Kalshi.above), priced by replication from its event's smile, and
   compared with Implied Odds' own probability. Exits with status 1 if any
   contract differs by more than 1e-9, so it can run in a script. *)

open! Core
open Contracts

let tolerance = 1e-9

let () =
  let dir =
    match Sys.get_argv () with
    | [| _; dir |] -> dir
    | _ ->
      eprintf "usage: check_export.exe EXPORT_DIR\n";
      exit 2
  in
  let pairs =
    (let open Or_error.Let_syntax in
     let%bind surfaces = Export.load_surfaces (dir ^/ "surfaces.csv")
     and rows = Export.load_rows (dir ^/ "contracts.csv") in
     Export.join surfaces rows)
    |> Or_error.ok_exn
  in
  let checked =
    List.map pairs ~f:(fun ((s : Export.Surface.t), (r : Export.Row.t)) ->
      let smile = s.smile in
      let price =
        Replication.price_exn
          (Replication.Model.of_smile smile)
          (Kalshi.above ~expiry:(Smile.years smile) ~strike:r.strike)
      in
      r, Float.abs (price -. r.p_opt))
  in
  let worst_row, worst =
    List.max_elt checked ~compare:(fun (_, x) (_, y) -> Float.compare x y)
    |> Option.value_exn
  in
  let failures = List.count checked ~f:(fun (_, e) -> Float.(e > tolerance)) in
  let snapshots =
    List.map pairs ~f:(fun (s, _) -> s.snap_utc) |> List.dedup_and_sort ~compare:String.compare
  in
  printf "snapshots:          %d (%s to %s)\n" (List.length snapshots)
    (List.hd_exn snapshots) (List.last_exn snapshots);
  printf "contracts checked:  %d\n" (List.length checked);
  printf "largest difference: %.2e (%s)\n" worst worst_row.ticker;
  printf "over %.0e:          %d\n" tolerance failures;
  if failures > 0 then exit 1
;;
