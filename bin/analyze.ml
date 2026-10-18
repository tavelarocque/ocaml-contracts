(* Kalshi's quotes against the options-implied probabilities.

     dune exec bin/analyze.exe -- ~/CODE/implied-odds/data/export/ocaml OUT_DIR

   Refuses to run before 2026-11-30: see the seal below.

   Prints a summary and writes CSVs to OUT_DIR: rows (every contract
   quote), quotes (the usable two-sided ones), buckets, events (one per
   snapshot and event), shape, actionable and summary. Fees assume a 100-contract taker
   order; the summary also shows the single-contract case, where the fee
   is rounded up to a whole cent. *)

open! Core
open Contracts

let kind_name k = Sexp.to_string [%sexp (k : Export.Kind.t)] |> String.lowercase
let opt = Option.value_map ~default:"" ~f:(sprintf "%.6f")

let write_csv file header lines =
  Out_channel.write_lines file (String.concat ~sep:"," header :: lines)
;;

let () =
  let export_dir, out_dir =
    match Sys.get_argv () with
    | [| _; e; o |] -> e, o
    | _ ->
      eprintf "usage: analyze.exe EXPORT_DIR OUT_DIR\n";
      exit 2
  in
  (* Implied Odds' layer 5 pre-registered this comparison and sealed its
     verdict until 2026-11-30; the exports cover that window. Until then the
     analysis runs only on synthetic data (test/test_analysis.ml). From that
     date this program is an independent second implementation of the
     verdict. 1_795_996_800 is 2026-11-30 00:00 UTC in seconds. *)
  let unsealed_at = 1_795_996_800. in
  if Float.(Time_ns.Span.to_sec (Time_ns.to_span_since_epoch (Time_ns.now ())) < unsealed_at)
  then (
    eprintf "The Kalshi-versus-options gap analysis is sealed until 2026-11-30\n";
    eprintf "(Implied Odds layer 5). Until then, use the export only for pricing\n";
    eprintf "(check_export.exe, touch.exe); the analysis is tested on synthetic data.\n";
    exit 3);
  if not (Stdlib.Sys.file_exists out_dir) then Stdlib.Sys.mkdir out_dir 0o755;
  let pairs =
    (let open Or_error.Let_syntax in
     let%bind surfaces = Export.load_surfaces (export_dir ^/ "surfaces.csv")
     and rows = Export.load_rows (export_dir ^/ "contracts.csv") in
     Export.join surfaces rows)
    |> Or_error.ok_exn
  in
  let rows = Report.analyze ~contracts:100 pairs in
  let single = Report.analyze ~contracts:1 pairs in
  let actionable_100 = Report.actionable ~contracts:100 rows in
  let actionable_1 = Report.actionable ~contracts:1 single in
  (* --- Data --- *)
  let count f = List.count rows ~f in
  let snapshots =
    List.map pairs ~f:(fun (s, _) -> s.snap_utc) |> List.dedup_and_sort ~compare:String.compare
  in
  printf "DATA\n";
  printf "  %d snapshots, %s to %s\n" (List.length snapshots) (List.hd_exn snapshots)
    (List.last_exn snapshots);
  printf "  %d contract quotes; %d inside the quoted option range; %d flagged as kink-sensitive\n"
    (List.length rows) (count (fun r -> r.row.in_range)) (count (fun r -> r.kink_sensitive));
  printf "  %d usable with a two-sided quote\n\n"
    (count (fun r -> r.usable && Option.is_some r.mid));
  (* --- Buckets --- *)
  let buckets = Report.buckets rows in
  printf "KALSHI MID MINUS MODEL, BY MODEL PROBABILITY (cents)\n";
  printf "  %-7s %-11s %6s %8s %8s %12s\n" "kind" "model P" "quotes" "mean" "median" "half-spread";
  List.iter buckets ~f:(fun b ->
    printf "  %-7s %4.2f-%4.2f  %6d %+8.2f %+8.2f %12.2f\n" (kind_name b.kind) b.lo b.hi b.n
      (100. *. b.mean_gap) (100. *. b.median_gap) (100. *. b.mean_half_spread));
  (* --- Shape --- *)
  let shapes = Report.shapes rows in
  printf "\nSHAPE OF KALSHI'S IMPLIED DISTRIBUTION vs THE OPTIONS'\n";
  List.iter (Report.shape_summary shapes) ~f:(fun s ->
    printf
      "  %-7s %3d snapshots of %3d events: IQR ratio median %.2f (quartiles %.2f-%.2f), wider in %.0f%%, median shift %+.0f USD; method alone stretches by %.3f\n"
      (kind_name s.kind) s.snapshots s.events s.median_iqr_ratio s.iqr_ratio_p25
      s.iqr_ratio_p75 (100. *. s.share_wider) s.median_shift s.median_method_bias);
  (* --- Actionable --- *)
  let show_actionable title actionable =
    printf "\n%s\n" title;
    List.iter actionable ~f:(fun (a : Report.Actionable.t) ->
      printf "  %-7s %-8s %5d quotes, %4d distinct contracts, %3d events, mean edge %.2f cents\n"
        (kind_name a.kind)
        (match a.side with
         | `Buy_yes -> "buy YES"
         | `Sell_yes -> "sell YES")
        a.rows a.contracts a.events (100. *. a.mean_edge))
  in
  show_actionable
    "QUOTES THAT BEAT THE FEE UNDER THE MODEL (model 5-95%, 100-contract orders, 100+ at the touch)"
    actionable_100;
  show_actionable "...AND FOR SINGLE-CONTRACT ORDERS (fee rounded up to a whole cent)" actionable_1;
  (* --- Files --- *)
  write_csv (out_dir ^/ "rows.csv")
    [ "snap_utc"; "event"; "kind"; "hours_to_close"; "ticker"; "strike"; "log_moneyness"
    ; "q_model"; "bid"; "ask"; "mid"; "gap"; "edge_buy_yes"; "edge_sell_yes"; "in_range"
    ; "kink_sensitive"; "usable" ]
    (List.map rows ~f:(fun r ->
       let s = r.surface in
       String.concat ~sep:","
         [ s.snap_utc; s.event; kind_name s.kind; sprintf "%.4f" s.hours_to_close
         ; r.row.ticker; sprintf "%.2f" r.row.strike
         ; sprintf "%.6f" (Float.log (r.row.strike /. Smile.forward s.smile))
         ; sprintf "%.6f" r.q; opt r.row.bid; opt r.row.ask; opt r.mid
         ; opt (Option.map r.mid ~f:(fun m -> m -. r.q)); opt r.edge_buy_yes
         ; opt r.edge_sell_yes; Bool.to_string r.row.in_range
         ; Bool.to_string r.kink_sensitive; Bool.to_string r.usable ]));
  write_csv (out_dir ^/ "buckets.csv")
    [ "kind"; "lo"; "hi"; "quotes"; "mean_gap"; "median_gap"; "mean_half_spread" ]
    (List.map buckets ~f:(fun b ->
       sprintf "%s,%.2f,%.2f,%d,%.6f,%.6f,%.6f" (kind_name b.kind) b.lo b.hi b.n b.mean_gap
         b.median_gap b.mean_half_spread));
  write_csv (out_dir ^/ "events.csv")
    [ "snap_utc"; "event"; "kind"; "hours_to_close"; "kalshi_iqr"; "model_iqr"
    ; "model_iqr_exact"; "iqr_ratio"; "kalshi_median"; "model_median"; "median_shift" ]
    (List.map shapes ~f:(fun ((s : Export.Surface.t), e) ->
       sprintf "%s,%s,%s,%.4f,%.2f,%.2f,%.2f,%.4f,%.2f,%.2f,%.2f" s.snap_utc s.event
         (kind_name s.kind) s.hours_to_close e.kalshi_iqr e.model_iqr e.model_iqr_exact
         (e.kalshi_iqr /. e.model_iqr) e.kalshi_median e.model_median
         (e.kalshi_median -. e.model_median)));
  let side_name = function
    | `Buy_yes -> "buy_yes"
    | `Sell_yes -> "sell_yes"
  in
  write_csv (out_dir ^/ "actionable.csv")
    [ "kind"; "side"; "order_contracts"; "quotes"; "contracts"; "events"; "mean_edge" ]
    (List.concat_map [ 100, actionable_100; 1, actionable_1 ] ~f:(fun (size, actionable) ->
       List.map actionable ~f:(fun (a : Report.Actionable.t) ->
         sprintf "%s,%s,%d,%d,%d,%d,%.6f" (kind_name a.kind) (side_name a.side) size a.rows
           a.contracts a.events a.mean_edge)));
  write_csv (out_dir ^/ "shape.csv")
    [ "kind"; "snapshots"; "events"; "median_iqr_ratio"; "iqr_ratio_p25"; "iqr_ratio_p75"
    ; "share_wider"; "median_shift"; "median_method_bias" ]
    (List.map (Report.shape_summary shapes) ~f:(fun s ->
       sprintf "%s,%d,%d,%.4f,%.4f,%.4f,%.4f,%.2f,%.4f" (kind_name s.kind) s.snapshots s.events
         s.median_iqr_ratio s.iqr_ratio_p25 s.iqr_ratio_p75 s.share_wider s.median_shift
         s.median_method_bias));
  write_csv (out_dir ^/ "summary.csv")
    [ "snapshots"; "first_snapshot"; "last_snapshot"; "quotes"; "in_range"; "kink_sensitive"
    ; "usable_two_sided" ]
    [ sprintf "%d,%s,%s,%d,%d,%d,%d" (List.length snapshots) (List.hd_exn snapshots)
        (List.last_exn snapshots) (List.length rows) (count (fun r -> r.row.in_range))
        (count (fun r -> r.kink_sensitive)) (count (fun r -> r.usable && Option.is_some r.mid))
    ];
  (* Only the usable two-sided quotes, for charts: the full rows.csv is
     larger than a page needs. *)
  write_csv (out_dir ^/ "quotes.csv")
    [ "snap_utc"; "kind"; "ticker"; "strike"; "q_model"; "bid"; "ask"; "mid"; "gap" ]
    (List.filter_map rows ~f:(fun r ->
       match r.mid, r.row.bid, r.row.ask with
       | Some mid, Some bid, Some ask when r.usable ->
         Some
           (sprintf "%s,%s,%s,%.2f,%.6f,%.2f,%.2f,%.3f,%.6f" r.surface.snap_utc
              (kind_name r.surface.kind) r.row.ticker r.row.strike r.q bid ask mid (mid -. r.q))
       | _ -> None));
  printf "\nwrote rows, buckets, events, actionable, shape, summary and quotes CSVs to %s\n" out_dir
;;
