open! Core

let year = 365. *. 86400.

module Fair = struct
  type t =
    { smiles : (float * Smile.t) array String.Map.t (* by event, sorted by snapshot time *)
    ; index : (float * float) array
    ; max_smile_age : float
    ; max_index_age : float
    }

  (* The last element whose time is at or before [at], by binary search. *)
  let latest arr at ~time =
    let rec go lo hi =
      (* invariant: arr.(lo) <= at < arr.(hi), with -1 and length as sentinels *)
      if hi - lo <= 1 then lo else (
        let mid = (lo + hi) / 2 in
        if Float.(time arr.(mid) <= at) then go mid hi else go lo mid)
    in
    let i = go (-1) (Array.length arr) in
    if i < 0 then None else Some arr.(i)
  ;;

  let create ?(max_smile_age = 75. *. 60.) ?(max_index_age = 30.) ~surfaces ~index () =
    let open Or_error.Let_syntax in
    let%map pairs =
      List.map surfaces ~f:(fun (s : Export.Surface.t) ->
        let%map t = Hf.parse_utc s.snap_utc in
        s.event, (t, s.smile))
      |> Or_error.combine_errors
    in
    let smiles =
      String.Map.of_alist_multi pairs
      |> Map.map ~f:(fun l ->
        Array.of_list l |> Array.sorted_copy ~compare:(fun (a, _) (b, _) -> Float.compare a b))
    in
    let index = Array.sorted_copy index ~compare:(fun (a, _) (b, _) -> Float.compare a b) in
    { smiles; index; max_smile_age; max_index_age }
  ;;

  let index_at t at =
    match latest t.index at ~time:fst with
    | Some (ti, x) when Float.(at -. ti <= t.max_index_age) -> Some x
    | _ -> None
  ;;

  let digital t ~event ~strike ~close ~at =
    let open Option.Let_syntax in
    let%bind index = index_at t at in
    if Float.(at >= close)
    then Some (if Float.(index > strike) then 1. else 0.)
    else (
      let%bind smiles = Map.find t.smiles event in
      let%bind snap, smile = latest smiles at ~time:fst in
      if Float.(at -. snap > t.max_smile_age)
      then None
      else (
        (* Move the forward by the index's own change since the snapshot.
           Using the same price series at both ends matters: Kraken's price
           and Deribit's index differ by tens of dollars, which would shift a
           last-hour digital by cents. Fall back to the smile's own spot
           only when the series has nothing near the snapshot. *)
        let at_snap = Option.value (index_at t snap) ~default:(Smile.spot smile) in
        let forward = Smile.forward smile *. index /. at_snap in
        let years = (close -. at) /. year in
        let moved =
          Smile.with_years (Smile.with_forward smile ~forward ~sticky:`Moneyness) ~years
        in
        (* The export's step, F * 1e-4, is $8 or so: fine a day out, coarse
           minutes before the close, when the whole distribution is a few
           tens of dollars wide. A hundredth of the distribution's width
           keeps the finite difference's error near 1e-6 at any horizon. *)
        let width = Smile.atm_vol moved *. Float.sqrt years in
        let relative_step = Float.min 1e-4 (width /. 100.) in
        Replication.price
          (Replication.Model.of_smile ~relative_step moved)
          (Kalshi.above ~expiry:years ~strike)
        |> Result.ok))
  ;;
end

type t =
  { trade : Hf.Trade.t
  ; event : string
  ; strike : float
  ; close : float
  ; after : float option array
  ; at_close : float option
  }
[@@deriving sexp_of]

let compute fair ~books ~trades ~horizons =
  let markets =
    List.fold books ~init:String.Map.empty ~f:(fun acc (b : Hf.Book.t) ->
      Map.set acc ~key:b.ticker ~data:(b.event, b.strike, b.close))
  in
  List.filter_map trades ~f:(fun (tr : Hf.Trade.t) ->
    let%map.Option event, strike, close = Map.find markets tr.ticker in
    let mark v =
      if tr.taker_bought_yes then tr.price -. v else v -. tr.price
    in
    let value at = Fair.digital fair ~event ~strike ~close ~at in
    { trade = tr
    ; event
    ; strike
    ; close
    ; after =
        Array.of_list horizons
        |> Array.map ~f:(fun h -> Option.map (value (tr.t +. h)) ~f:mark)
    ; at_close = Option.map (value close) ~f:mark
    })
;;

module Summary = struct
  type row =
    { label : string
    ; trades : int
    ; contracts : float
    ; mean : float
    ; se : float
    ; ci_low : float
    ; ci_high : float
    }
  [@@deriving sexp_of]

  (* (event, weight, value) triples -> one row. *)
  let row ~resamples ~seed label obs =
    let w = List.sum (module Float) obs ~f:(fun (_, w, _) -> w) in
    let mean = List.sum (module Float) obs ~f:(fun (_, w, x) -> w *. x) /. w in
    let groups =
      List.map obs ~f:(fun (e, w, x) -> e, (w, x))
      |> String.Map.of_alist_multi
      |> Map.data
      |> Array.of_list
    in
    (* Cluster-robust standard error of a weighted mean: residuals summed
       within each event before squaring. *)
    let se =
      Float.sqrt
        (Array.sum (module Float) groups ~f:(fun g ->
           Float.square (List.sum (module Float) g ~f:(fun (w, x) -> w *. (x -. mean)))))
      /. w
    in
    let rng = Random.State.make [| seed |] in
    let g = Array.length groups in
    let means =
      Array.init resamples ~f:(fun _ ->
        let sw = ref 0.
        and sx = ref 0. in
        for _ = 1 to g do
          List.iter groups.(Random.State.int rng g) ~f:(fun (w, x) ->
            sw := !sw +. w;
            sx := !sx +. (w *. x))
        done;
        !sx /. !sw)
    in
    Array.sort means ~compare:Float.compare;
    let q p = means.(Int.min (resamples - 1) (Float.iround_down_exn (p *. Float.of_int resamples))) in
    { label; trades = List.length obs; contracts = w; mean; se; ci_low = q 0.025; ci_high = q 0.975 }
  ;;

  let compute ?(resamples = 2000) ?(seed = 16) ~horizons ms =
    let pick f =
      List.filter_map ms ~f:(fun m ->
        Option.map (f m) ~f:(fun x -> m.event, m.trade.Hf.Trade.count, x))
    in
    let rows =
      List.mapi horizons ~f:(fun i h ->
        sprintf "%gs" h, pick (fun m -> m.after.(i)))
      @ [ "close", pick (fun m -> m.at_close) ]
    in
    List.filter_map rows ~f:(fun (label, obs) ->
      if List.is_empty obs then None else Some (row ~resamples ~seed label obs))
  ;;
end
