(* Markouts of Kalshi trades against options-implied fair value, and the
   market maker's quoting rule replayed against the real book.

     dune exec bin/markout.exe -- ~/CODE/implied-odds/data/export/ocaml_hf [MAX_INDEX_AGE]

   MAX_INDEX_AGE (seconds, default 30) is how old the latest BTC price may
   be. 30 suits both sources: the live collector's index every 5 seconds,
   and the backfill's Kraken price every second with a trade.

   Reads the high-frequency export (tools/export_hf_for_ocaml.py in Implied
   Odds). Refuses, with exit status 3, to compare any data inside a sealed
   window before that window opens (see lib/seal.mli): this is exactly the
   comparison those pre-registered tests will make first.

   Trades are filtered as the draft of layer 16 says: strike within 1% of
   the index, 15 minutes to 24 hours before the close, price 5 to 95 cents.
   The index stands in for each event's forward in the 1% filter. *)

open! Core
open Contracts

let horizons = [ 0.; 10.; 60.; 300. ]

let print_rows title rows =
  printf "\n%s\n" title;
  List.iter rows ~f:(fun (r : Markout.Summary.row) ->
    printf "  %-6s %6d trades %9.0f contracts: %+6.2f cents (se %.2f, 95%% %+.2f to %+.2f)\n"
      r.label r.trades r.contracts (100. *. r.mean) (100. *. r.se) (100. *. r.ci_low)
      (100. *. r.ci_high))
;;

let () =
  let dir, max_index_age =
    match Sys.get_argv () with
    | [| _; dir |] -> dir, 30.
    | [| _; dir; age |] -> dir, Float.of_string age
    | _ ->
      eprintf "usage: markout.exe EXPORT_DIR [MAX_INDEX_AGE_SECONDS]\n";
      exit 2
  in
  let load = Or_error.ok_exn in
  let books = load (Hf.load_books (dir ^/ "books.csv"))
  and trades = load (Hf.load_trades (dir ^/ "trades.csv"))
  and index = load (Hf.load_index (dir ^/ "index.csv"))
  and surfaces = load (Export.load_surfaces (dir ^/ "surfaces.csv")) in
  let now = Time_ns.to_span_since_epoch (Time_ns.now ()) |> Time_ns.Span.to_sec in
  (match
     Seal.check ~now
       (List.map trades ~f:(fun t -> t.Hf.Trade.t) @ List.map books ~f:(fun b -> b.Hf.Book.t))
   with
   | Ok () -> ()
   | Error e ->
     eprintf "%s.\nCollecting is fine; this comparison waits. Nothing was computed.\n" e;
     exit 3);
  let fair = load (Markout.Fair.create ~max_index_age ~surfaces ~index ()) in
  let all = Markout.compute fair ~books ~trades ~horizons in
  let keep (m : Markout.t) =
    let to_close = m.close -. m.trade.t in
    Float.(to_close >= 15. *. 60. && to_close <= 86400.)
    && Float.(m.trade.price >= 0.05 && m.trade.price <= 0.95)
    &&
    match Markout.Fair.index_at fair m.trade.t with
    | Some x -> Float.(abs (log (m.strike /. x)) <= 0.01)
    | None -> false
  in
  let used = List.filter all ~f:keep in
  printf "%d trades loaded, %d with a known market, %d pass the filters\n" (List.length trades)
    (List.length all) (List.length used);
  print_rows "Passive-side markout, all filtered trades" (Markout.Summary.compute ~horizons used);
  List.iter [ "15-60 minutes to close", 900., 3600.; "1-6 hours", 3600., 21600.; "6-24 hours", 21600., 86400.1 ]
    ~f:(fun (label, lo, hi) ->
      let part =
        List.filter used ~f:(fun m ->
          let d = m.close -. m.trade.t in
          Float.(d >= lo && d < hi))
      in
      if not (List.is_empty part) then print_rows label (Markout.Summary.compute ~horizons part));
  let params =
    { Market_maker.Params.gamma = 0.01; k = 50.; alpha = 0.; max_inventory = 50; hedge = false }
  in
  let fills =
    Replay.run fair params ~books ~trades:(List.map used ~f:(fun m -> m.trade)) ~horizons
  in
  printf "\nThe quoting rule (gamma 0.01, k 50) replayed against those trades: %d fills, P&L at the close $%+.2f\n"
    (List.length fills)
    (List.sum (module Float) fills ~f:(fun m -> Option.value m.at_close ~default:0.));
  printf "(fills assume our quote was first in the queue, so this P&L is an upper bound)\n";
  if List.is_empty fills
  then printf "No fills: its quotes were never at or inside the prices takers paid.\n"
  else print_rows "Markout of the replayed fills" (Markout.Summary.compute ~horizons fills)
;;
