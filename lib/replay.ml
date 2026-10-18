open! Core

let run fair (params : Market_maker.Params.t) ~books ~trades ~horizons =
  let books_by =
    List.map books ~f:(fun (b : Hf.Book.t) -> b.ticker, b)
    |> String.Map.of_alist_multi
    |> Map.map ~f:(fun l ->
      Array.of_list l |> Array.sorted_copy ~compare:(fun (a : Hf.Book.t) b -> Float.compare a.t b.t))
  in
  let trades_by =
    List.map trades ~f:(fun (t : Hf.Trade.t) -> t.ticker, t)
    |> String.Map.of_alist_multi
    |> Map.map ~f:(List.sort ~compare:(fun (a : Hf.Trade.t) b -> Float.compare a.t b.t))
  in
  Map.to_alist trades_by
  |> List.concat_map ~f:(fun (ticker, trades) ->
    match Map.find books_by ticker with
    | None -> []
    | Some books ->
      let first = books.(0) in
      let book_at at =
        (* The latest book at or before [at], by binary search. *)
        let rec go lo hi =
          if hi - lo <= 1 then lo else (
            let mid = (lo + hi) / 2 in
            if Float.(books.(mid).t <= at) then go mid hi else go lo mid)
        in
        let i = go (-1) (Array.length books) in
        if i < 0 then None else Some books.(i)
      in
      let value at =
        Markout.Fair.digital fair ~event:first.event ~strike:first.strike ~close:first.close ~at
      in
      let inventory = ref 0 in
      List.filter_map trades ~f:(fun (tr : Hf.Trade.t) ->
        let open Option.Let_syntax in
        let%bind fair_now = value tr.t in
        let bid, ask =
          Market_maker.quotes params ~fair:fair_now ~step_sd:0. ~inventory:!inventory
        in
        (* Don't post a quote that would trade against the book on arrival. *)
        let book = book_at tr.t in
        let bid =
          Option.filter bid ~f:(fun b ->
            match Option.bind book ~f:(fun (k : Hf.Book.t) -> k.ask) with
            | Some a -> Float.(b < a)
            | None -> true)
        and ask =
          Option.filter ask ~f:(fun a ->
            match Option.bind book ~f:(fun (k : Hf.Book.t) -> k.bid) with
            | Some b -> Float.(a > b)
            | None -> true)
        in
        let%map price, we_bought =
          if tr.taker_bought_yes
          then Option.bind ask ~f:(fun a -> Option.some_if Float.(tr.price >= a) (a, false))
          else Option.bind bid ~f:(fun b -> Option.some_if Float.(tr.price <= b) (b, true))
        in
        inventory := !inventory + if we_bought then 1 else -1;
        let mark v = if we_bought then v -. price else price -. v in
        { Markout.trade =
            { tr with price; count = 1.; taker_bought_yes = not we_bought }
        ; event = first.event
        ; strike = first.strike
        ; close = first.close
        ; after =
            Array.of_list horizons
            |> Array.map ~f:(fun h -> Option.map (value (tr.t +. h)) ~f:mark)
        ; at_close = Option.map (value first.close) ~f:mark
        }))
;;
