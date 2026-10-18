(* Markouts on a synthetic market where the answer is known. No real
   quotes or trades. *)

open! Core
open Contracts

let%expect_test "UTC times both ways, and the seal's opening date" =
  (* 2026-11-30T00:00Z is the constant bin/analyze.ml already seals on. *)
  List.iter [ "1970-01-01T00:00:00Z"; "2026-11-30T00:00:00Z"; "2026-10-17T15:04:05.25+00:00" ]
    ~f:(fun s ->
      let x = Hf.parse_utc s |> Or_error.ok_exn in
      printf "%-30s %16.2f  %s\n" s x (Hf.to_utc_string x));
  [%expect {|
    1970-01-01T00:00:00Z                       0.00  1970-01-01T00:00:00.000000+00:00
    2026-11-30T00:00:00Z              1795996800.00  2026-11-30T00:00:00.000000+00:00
    2026-10-17T15:04:05.25+00:00      1792249445.25  2026-10-17T15:04:05.250000+00:00
    |}]
;;

let%expect_test "the seal" =
  let at s = Hf.parse_utc s |> Or_error.ok_exn in
  let show now times =
    match Seal.check ~now:(at now) (List.map times ~f:at) with
    | Ok () -> printf "%s: open\n" now
    | Error e -> printf "%s: %s\n" now e
  in
  show "2026-10-17T00:00:00Z" [ "2026-10-20T12:00:00Z" ];
  show "2026-11-30T00:00:00Z" [ "2026-10-20T12:00:00Z" ];
  show "2026-11-30T00:00:00Z" [ "2026-12-05T12:00:00Z" ];
  show "2026-12-28T00:00:00Z" [ "2026-12-05T12:00:00Z" ];
  show "2026-10-17T00:00:00Z" [ "2030-01-01T00:00:00Z" ];
  [%expect {|
    2026-10-17T00:00:00Z: sealed: the data covers Implied Odds layer 5 (Kalshi vs options), which opens on 2026-11-30
    2026-11-30T00:00:00Z: open
    2026-11-30T00:00:00Z: sealed: the data covers Implied Odds layer 16 test window (draft), which opens on 2026-12-28
    2026-12-28T00:00:00Z: open
    2026-10-17T00:00:00Z: open
    |}]
;;

let index = lazy (Synthetic_hf.index_path ~seed:5)

let fair =
  lazy
    (Markout.Fair.create ~surfaces:(Synthetic_hf.surfaces (force index)) ~index:(force index) ()
     |> Or_error.ok_exn)
;;

let%expect_test "fair value: hourly smiles moved with the index match Black" =
  (* Between snapshots the fair value comes from the event's last smile,
     re-centred on the index and shortened to the time left. With a flat
     smile and a lognormal index that is exactly Black's N(d2), written out
     separately in Synthetic_hf. What's left is the finite difference
     inside each digital, which Markout scales to the distribution's width
     so it stays small right up to the close. *)
  let worst = ref 0.
  and missing = ref 0 in
  List.iter Synthetic_hf.events ~f:(fun e ->
    List.iter (List.range 1 (4 * 3600) ~stride:97) ~f:(fun s ->
      let at = e.close -. Float.of_int s in
      List.iter Synthetic_hf.strikes ~f:(fun strike ->
        match
          Markout.Fair.digital (force fair) ~event:e.name ~strike ~close:e.close ~at
        with
        | None -> incr missing
        | Some v ->
          worst :=
            Float.max !worst
              (Float.abs (v -. Synthetic_hf.true_value (force index) ~close:e.close ~strike ~at)))));
  printf "largest gap from Black: %.1e; times without a fair value: %d\n" !worst !missing;
  [%expect {| largest gap from Black: 4.1e-06; times without a fair value: 0 |}]
;;

let horizons = [ 0.; 10.; 60.; 300. ]

let show_summary ms =
  List.iter (Markout.Summary.compute ~horizons ms) ~f:(fun (r : Markout.Summary.row) ->
    printf "%-6s %5d trades: %+6.2f cents (se %.2f, 95%% %+.2f to %+.2f)\n" r.label r.trades
      (100. *. r.mean) (100. *. r.se) (100. *. r.ci_low) (100. *. r.ci_high))
;;

let run ~informed ~lookahead =
  Markout.compute (force fair) ~books:Synthetic_hf.books
    ~trades:(Synthetic_hf.trades (force index) ~informed ~lookahead ~seed:9)
    ~horizons
;;

let%expect_test "uninformed takers: the passive side keeps the half-spread at every horizon" =
  (* Takers here know nothing, so fair value after the trade is as likely
     to move for the passive side as against it. Its markout is the 2-cent
     half-spread at the trade and stays there, within its standard error,
     at every horizon. Standard errors are clustered by event, since trades
     in one event share a price path.

     The close is the noisiest, a $0/$1 payout, and here its 95% interval
     misses the true 2 cents. Eight events are too few clusters for a
     bootstrap to be trusted, and every event shares one price path. That
     is why the draft of layer 16 decides at 60 seconds, not the close, and
     asks for at least 20 days of trades. *)
  show_summary (run ~informed:0. ~lookahead:60.);
  [%expect {|
    0s      2517 trades:  +2.00 cents (se 0.00, 95% +2.00 to +2.00)
    10s     2517 trades:  +2.02 cents (se 0.02, 95% +2.00 to +2.06)
    60s     2517 trades:  +1.94 cents (se 0.06, 95% +1.84 to +2.06)
    300s    2517 trades:  +2.04 cents (se 0.13, 95% +1.75 to +2.26)
    close   2517 trades:  +0.31 cents (se 0.74, 95% -1.08 to +1.74)
    |}]
;;

let%expect_test "informed takers: the markout falls with the horizon and turns negative" =
  (* Nine takers in ten now see fair value a minute ahead, and trade only
     when that beats the spread. At the moment of the trade nothing looks
     different: the passive side still earned 2 cents against fair value
     then. Ten seconds later it's 1.55, a minute later -0.97, and the 95%
     interval is below zero: "takers are informed" in the draft's terms.
     After a minute the informed traders know nothing more, so the markout
     stops falling, within its noise. That's the whole method: it tells
     informed flow from uninformed without knowing who anyone is. With
     half the takers informed (not shown) the minute markout is +0.97:
     smaller than the spread, still positive. The method measures how
     much, not just whether. *)
  show_summary (run ~informed:0.9 ~lookahead:60.);
  [%expect {|
    0s      1195 trades:  +2.00 cents (se 0.00, 95% +2.00 to +2.00)
    10s     1195 trades:  +1.55 cents (se 0.04, 95% +1.47 to +1.64)
    60s     1195 trades:  -0.97 cents (se 0.12, 95% -1.17 to -0.69)
    300s    1195 trades:  -1.34 cents (se 0.13, 95% -1.64 to -1.09)
    close   1195 trades:  -3.01 cents (se 1.74, 95% -6.82 to +0.31)
    |}]
;;

let%expect_test "replaying the quoting rule against the same synthetic trades" =
  (* The market maker quotes about a cent either side of fair value
     (k = 100), so it is filled by any taker paying the 2-cent half-spread
     whose side its quote is on. Against uninformed takers its fills keep
     their edge at every horizon. Against the mostly informed takers its
     fills lose within a minute: being first in the queue is no help when
     the people hitting you know more. The sum of the close markouts is the
     replay's P&L: +$23 against the uninformed, -$28 against the informed,
     on one contract a fill.

     The close is much less noisy here than in the takers' markouts above
     (standard error 0.21 cents, not 0.74), because the market maker leans
     against its inventory: its buys and sells in a market roughly cancel,
     so little of its P&L rides on where BTC settles. *)
  let params =
    { Market_maker.Params.gamma = 0.01; k = 100.; alpha = 0.; max_inventory = 50; hedge = false }
  in
  List.iter [ "uninformed", 0.; "90% informed", 0.9 ] ~f:(fun (label, informed) ->
    let fills =
      Replay.run (force fair) params ~books:Synthetic_hf.books
        ~trades:(Synthetic_hf.trades (force index) ~informed ~lookahead:60. ~seed:9)
        ~horizons
    in
    let pnl = List.sum (module Float) fills ~f:(fun m -> Option.value m.at_close ~default:0.) in
    printf "%s: %d fills, P&L at the close $%+.2f\n" label (List.length fills) pnl;
    show_summary fills);
  [%expect {|
    uninformed: 2137 fills, P&L at the close $+23.04
    0s      2137 trades:  +1.45 cents (se 0.01, 95% +1.44 to +1.46)
    10s     2137 trades:  +1.45 cents (se 0.03, 95% +1.40 to +1.52)
    60s     2137 trades:  +1.41 cents (se 0.05, 95% +1.31 to +1.51)
    300s    2137 trades:  +1.66 cents (se 0.12, 95% +1.38 to +1.86)
    close   2137 trades:  +1.08 cents (se 0.21, 95% +0.67 to +1.51)
    90% informed: 966 fills, P&L at the close $-28.31
    0s       966 trades:  +1.46 cents (se 0.01, 95% +1.44 to +1.47)
    10s      966 trades:  +1.02 cents (se 0.05, 95% +0.93 to +1.13)
    60s      966 trades:  -1.37 cents (se 0.15, 95% -1.63 to -1.03)
    300s     966 trades:  -1.36 cents (se 0.22, 95% -1.80 to -0.93)
    close    966 trades:  -2.93 cents (se 0.80, 95% -4.79 to -1.56)
    |}]
;;

let%expect_test "the export's CSV format, written and read back" =
  (* The synthetic market written in the export's format and loaded again
     must give the same markouts. bin/markout.exe can then be tried on
     this folder (dates in 2030 are outside every sealed window). *)
  let dir = "synthetic_hf_export" in
  if not (Stdlib.Sys.file_exists dir) then Stdlib.Sys.mkdir dir 0o755;
  let ix = force index in
  let write name header rows =
    Out_channel.write_lines (dir ^/ name) (header :: rows)
  in
  let g = sprintf "%.17g" in
  write "index.csv" "t,index"
    (Array.to_list ix |> List.map ~f:(fun (t, x) -> Hf.to_utc_string t ^ "," ^ g x));
  write "books.csv" "t,event,ticker,strike,close_utc,bid,ask,bid_size,ask_size"
    (List.map Synthetic_hf.books ~f:(fun (b : Hf.Book.t) ->
       String.concat ~sep:","
         [ Hf.to_utc_string b.t; b.event; b.ticker; g b.strike; Hf.to_utc_string b.close; ""; ""; ""; "" ]));
  let trades = Synthetic_hf.trades ix ~informed:0.9 ~lookahead:60. ~seed:9 in
  write "trades.csv" "t,ticker,price,count,taker_side"
    (List.map trades ~f:(fun (t : Hf.Trade.t) ->
       String.concat ~sep:","
         [ Hf.to_utc_string t.t; t.ticker; g t.price; g t.count
         ; (if t.taker_bought_yes then "yes" else "no") ]));
  write "surfaces.csv" "snap_utc,event,close_utc,kind,hours_to_close,spot,F,T,a,b,rho,m,s,k_min,k_max"
    (List.map (Synthetic_hf.surfaces ix) ~f:(fun (s : Export.Surface.t) ->
       let p = Smile.params s.smile in
       String.concat ~sep:","
         [ s.snap_utc; s.event; s.close_utc; "hourly"; g s.hours_to_close; g (Smile.spot s.smile)
         ; g (Smile.forward s.smile); g (Smile.years s.smile); g p.a; g p.b; g p.rho; g p.m; g p.s
         ; "-1"; "1" ]));
  let load = Or_error.ok_exn in
  let fair' =
    load
      (Markout.Fair.create
         ~surfaces:(load (Export.load_surfaces (dir ^/ "surfaces.csv")))
         ~index:(load (Hf.load_index (dir ^/ "index.csv")))
         ())
  in
  let from_disk =
    Markout.compute fair' ~books:(load (Hf.load_books (dir ^/ "books.csv")))
      ~trades:(load (Hf.load_trades (dir ^/ "trades.csv"))) ~horizons
  and in_memory = Markout.compute (force fair) ~books:Synthetic_hf.books ~trades ~horizons in
  let worst =
    List.zip_exn from_disk in_memory
    |> List.fold ~init:0. ~f:(fun acc (a, b) ->
      Array.fold2_exn a.after b.after ~init:acc ~f:(fun acc x y ->
        match x, y with
        | Some x, Some y -> Float.max acc (Float.abs (x -. y))
        | _ -> Float.infinity))
  in
  printf "%d trades; largest difference after the round trip: %.1e\n" (List.length from_disk) worst;
  [%expect {| 1195 trades; largest difference after the round trip: 0.0e+00 |}]
;;

let%expect_test "a different price source, and 1-minute prices" =
  (* The backfill's BTC price is not the Deribit index the smiles were
     fitted against. Two things could go wrong.

     A different source sits at a slightly different level (here 0.03%
     higher, about $30). Because fair value moves the forward by the index's
     own change since the snapshot, a level difference cancels: the gap
     from the true value is unchanged.

     Prices sampled once a minute are coarser in time: fair value then uses
     a price up to a minute old. In the last hour a minute of BTC moves a
     digital by cents, and that is the error printed, about a cent on
     average. It's why the backfill takes Kraken's trades (a price every
     second) rather than Coinbase's 1-minute candles. *)
  let ix = force index in
  let gap index' ~max_index_age =
    let fair' =
      Markout.Fair.create ~max_index_age ~surfaces:(Synthetic_hf.surfaces ix) ~index:index' ()
      |> Or_error.ok_exn
    in
    let worst = ref 0.
    and n = ref 0
    and sum = ref 0. in
    List.iter Synthetic_hf.events ~f:(fun e ->
      List.iter (List.range 900 (4 * 3600) ~stride:61) ~f:(fun s ->
        let at = e.close -. Float.of_int s in
        List.iter Synthetic_hf.strikes ~f:(fun strike ->
          match Markout.Fair.digital fair' ~event:e.name ~strike ~close:e.close ~at with
          | None -> ()
          | Some v ->
            let d = Float.abs (v -. Synthetic_hf.true_value ix ~close:e.close ~strike ~at) in
            worst := Float.max !worst d;
            sum := !sum +. d;
            incr n)));
    printf "mean gap %.4f, worst %.4f (%d points)\n" (!sum /. Float.of_int !n) !worst !n
  in
  printf "same source, every second:      ";
  gap ix ~max_index_age:30.;
  printf "source 0.03%% higher, every second: ";
  gap (Array.map ix ~f:(fun (t, x) -> t, x *. 1.0003)) ~max_index_age:30.;
  printf "same source, 1-minute closes:   ";
  gap
    (Array.filter_mapi ix ~f:(fun i (t, x) -> Option.some_if (i % 60 = 59) (t +. 1., x)))
    ~max_index_age:90.;
  [%expect {|
    same source, every second:      mean gap 0.0000, worst 0.0000 (15984 points)
    source 0.03% higher, every second: mean gap 0.0000, worst 0.0000 (15984 points)
    same source, 1-minute closes:   mean gap 0.0095, worst 0.1921 (15984 points)
    |}]
;;
