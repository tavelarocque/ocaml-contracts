open! Core

module Row = struct
  type t =
    { surface : Export.Surface.t
    ; row : Export.Row.t
    ; q : float
    ; kink_sensitive : bool
    ; usable : bool
    ; mid : float option
    ; half_spread : float option
    ; edge_buy_yes : float option
    ; edge_sell_yes : float option
    }
end

let analyze ~contracts pairs =
  List.map pairs ~f:(fun ((surface : Export.Surface.t), (row : Export.Row.t)) ->
    let smile = surface.smile in
    let q =
      Replication.price_exn
        (Replication.Model.of_smile smile)
        (Kalshi.above ~expiry:(Smile.years smile) ~strike:row.strike)
    in
    let kink_sensitive =
      Float.(
        abs (Smile.prob_above smile row.strike
             -. Smile.prob_above ~relative_step:1e-6 smile row.strike)
        > 0.005)
    in
    let two_sided =
      match row.bid, row.ask with
      | Some bid, Some ask -> Option.some_if Float.(bid > 0.) (bid, ask)
      | _ -> None
    in
    { Row.surface
    ; row
    ; q
    ; kink_sensitive
    ; usable = row.in_range && not kink_sensitive
    ; mid = Option.map two_sided ~f:(fun (bid, ask) -> (bid +. ask) /. 2.)
    ; half_spread = Option.map two_sided ~f:(fun (bid, ask) -> (ask -. bid) /. 2.)
    ; edge_buy_yes = Analysis.edge_buy_yes ~q ~ask:row.ask ~contracts
    ; edge_sell_yes = Analysis.edge_sell_yes ~q ~bid:row.bid ~contracts
    })
;;

let quantile xs p =
  let a = List.sort xs ~compare:Float.compare |> Array.of_list in
  let n = Array.length a in
  let pos = p *. Float.of_int (n - 1) in
  let i = Float.iround_down_exn pos in
  if i >= n - 1 then a.(n - 1) else a.(i) +. ((pos -. Float.of_int i) *. (a.(i + 1) -. a.(i)))
;;

let mean xs = List.sum (module Float) xs ~f:Fn.id /. Float.of_int (List.length xs)
let kinds = [ Export.Kind.Hourly; Daily ]

module Bucket = struct
  type t =
    { kind : Export.Kind.t
    ; lo : float
    ; hi : float
    ; n : int
    ; mean_gap : float
    ; median_gap : float
    ; mean_half_spread : float
    }
end

let edges = [ 0.; 0.05; 0.15; 0.35; 0.65; 0.85; 0.95; 1. ]

let buckets rows =
  let ranges = List.zip_exn (List.drop_last_exn edges) (List.tl_exn edges) in
  List.concat_map kinds ~f:(fun kind ->
    List.filter_map ranges ~f:(fun (lo, hi) ->
      let inside =
        List.filter_map rows ~f:(fun (r : Row.t) ->
          match r.mid, r.half_spread with
          | Some mid, Some hs
            when r.usable
                 && Export.Kind.equal r.surface.kind kind
                 && Float.(r.q >= lo)
                 && (Float.(r.q < hi) || Float.(hi = 1. && r.q <= 1.)) ->
            Some (mid -. r.q, hs)
          | _ -> None)
      in
      if List.is_empty inside
      then None
      else (
        let gaps = List.map inside ~f:fst in
        Some
          { Bucket.kind
          ; lo
          ; hi
          ; n = List.length inside
          ; mean_gap = mean gaps
          ; median_gap = quantile gaps 0.5
          ; mean_half_spread = mean (List.map inside ~f:snd)
          })))
;;

module Actionable = struct
  type t =
    { kind : Export.Kind.t
    ; side : [ `Buy_yes | `Sell_yes ]
    ; rows : int
    ; contracts : int
    ; events : int
    ; mean_edge : float
    }
end

let count_distinct xs = List.dedup_and_sort xs ~compare:String.compare |> List.length

let actionable ~contracts rows =
  List.concat_map kinds ~f:(fun kind ->
    List.filter_map [ `Buy_yes; `Sell_yes ] ~f:(fun side ->
      let hits =
        List.filter_map rows ~f:(fun (r : Row.t) ->
          let edge =
            match side with
            | `Buy_yes -> r.edge_buy_yes
            | `Sell_yes -> r.edge_sell_yes
          in
          let size =
            match side with
            | `Buy_yes -> r.row.ask_size
            | `Sell_yes -> r.row.bid_size
          in
          let enough = Option.value_map size ~default:false ~f:(fun n -> Float.(n >= of_int contracts)) in
          match edge with
          | Some e
            when r.usable
                 && enough
                 && Float.(r.q >= 0.05 && r.q <= 0.95)
                 && Export.Kind.equal r.surface.kind kind
                 && Float.(e > 0.) -> Some (r, e)
          | _ -> None)
      in
      if List.is_empty hits
      then None
      else
        Some
          { Actionable.kind
          ; side
          ; rows = List.length hits
          ; contracts = count_distinct (List.map hits ~f:(fun (r, _) -> r.row.ticker))
          ; events = count_distinct (List.map hits ~f:(fun (r, _) -> r.row.event))
          ; mean_edge = mean (List.map hits ~f:snd)
          }))
;;

module Shape = struct
  type t =
    { kind : Export.Kind.t
    ; snapshots : int
    ; events : int
    ; median_iqr_ratio : float
    ; iqr_ratio_p25 : float
    ; iqr_ratio_p75 : float
    ; share_wider : float
    ; median_shift : float
    ; median_method_bias : float
    }
end

let shapes rows =
  List.filter_map rows ~f:(fun (r : Row.t) ->
    Option.map r.mid ~f:(fun mid -> r.surface.snap_utc ^ "|" ^ r.surface.event, (r, mid)))
  |> List.filter ~f:(fun (_, ((r : Row.t), _)) -> r.usable)
  |> String.Map.of_alist_multi
  |> Map.data
  |> List.filter_map ~f:(fun group ->
    let (r : Row.t), _ = List.hd_exn group in
    let points = List.map group ~f:(fun ((r : Row.t), mid) -> r.row.strike, mid, r.q) in
    Analysis.Event_shape.compute r.surface.smile points
    |> Option.map ~f:(fun shape -> r.surface, shape))
;;

let shape_summary shapes =
  List.filter_map kinds ~f:(fun kind ->
    let these =
      List.filter shapes ~f:(fun ((s : Export.Surface.t), _) -> Export.Kind.equal s.kind kind)
    in
    if List.is_empty these
    then None
    else (
      let ratios =
        List.map these ~f:(fun (_, (e : Analysis.Event_shape.t)) -> e.kalshi_iqr /. e.model_iqr)
      in
      let shifts =
        List.map these ~f:(fun (_, (e : Analysis.Event_shape.t)) ->
          e.kalshi_median -. e.model_median)
      in
      Some
        { Shape.kind
        ; snapshots = List.length these
        ; events = count_distinct (List.map these ~f:(fun (s, _) -> s.event))
        ; median_iqr_ratio = quantile ratios 0.5
        ; iqr_ratio_p25 = quantile ratios 0.25
        ; iqr_ratio_p75 = quantile ratios 0.75
        ; share_wider =
            Float.of_int (List.count ratios ~f:(fun x -> Float.(x > 1.)))
            /. Float.of_int (List.length ratios)
        ; median_shift = quantile shifts 0.5
        ; median_method_bias =
            quantile
              (List.map these ~f:(fun (_, (e : Analysis.Event_shape.t)) ->
                 e.model_iqr /. e.model_iqr_exact))
              0.5
        }))
;;
