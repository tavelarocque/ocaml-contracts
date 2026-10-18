open! Core

let taker_fee ~price ~contracts =
  let raw = 0.07 *. price *. (1. -. price) *. Float.of_int contracts in
  (* Round the order's total up to the cent. Rounding [raw * 100] to 9
     decimals first stops float noise (1.75000000001) from adding a cent. *)
  let cents = Float.round_up (Float.round_decimal (raw *. 100.) ~decimal_digits:9) in
  cents /. 100. /. Float.of_int contracts
;;

let edge_buy_yes ~q ~ask ~contracts =
  Option.map ask ~f:(fun ask -> q -. ask -. taker_fee ~price:ask ~contracts)
;;

let edge_sell_yes ~q ~bid ~contracts =
  Option.bind bid ~f:(fun bid ->
    (* A zero bid means nobody is buying: there is nothing to sell into. *)
    if Float.(bid <= 0.) then None else Some (bid -. q -. taker_fee ~price:bid ~contracts))
;;

let normal_cdf x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.)

let probit p =
  (* Bisection: normal_cdf rises from 0 to 1, so keep the half where it
     crosses p. 200 halvings of [-40, 40] are far more than enough. *)
  let rec go lo hi n =
    if n = 0
    then (lo +. hi) /. 2.
    else (
      let mid = (lo +. hi) /. 2. in
      if Float.(normal_cdf mid < p) then go mid hi (n - 1) else go lo mid (n - 1))
  in
  go (-40.) 40. 200
;;

let ladder_quantile points p =
  let sorted = List.sort points ~compare:(fun (k1, _) (k2, _) -> Float.compare k1 k2) in
  let monotone =
    List.folding_map sorted ~init:Float.infinity ~f:(fun lowest (k, v) ->
      let v = Float.min lowest v in
      v, (k, v))
  in
  let interior v = Float.(v > 0. && v < 1.) in
  List.zip_exn (List.drop_last monotone |> Option.value ~default:[])
    (List.tl monotone |> Option.value ~default:[])
  |> List.find_map ~f:(fun ((k1, v1), (k2, v2)) ->
    if Float.(v1 >= p && p >= v2 && v1 > v2)
    then (
      (* A price of exactly 0 or 1 has no normal quantile; such a segment
         falls back to straight-line interpolation. *)
      let t =
        if interior v1 && interior v2
        then (probit v1 -. probit p) /. (probit v1 -. probit v2)
        else (v1 -. p) /. (v1 -. v2)
      in
      Some (k1 +. (t *. (k2 -. k1))))
    else None)
;;

module Event_shape = struct
  type t =
    { kalshi_iqr : float
    ; model_iqr : float
    ; model_iqr_exact : float
    ; kalshi_median : float
    ; model_median : float
    }
  [@@deriving sexp_of]

  (* The strikes where an "above K" curve crosses 75%, 50% and 25%: the
     25th percentile, median and 75th percentile of the price. *)
  let quartiles points =
    let open Option.Let_syntax in
    let%map lower = ladder_quantile points 0.75
    and median = ladder_quantile points 0.5
    and upper = ladder_quantile points 0.25 in
    upper -. lower, median
  ;;

  let compute smile points =
    let open Option.Let_syntax in
    let%map kalshi_iqr, kalshi_median = quartiles (List.map points ~f:(fun (k, mid, _) -> k, mid))
    and model_iqr, model_median = quartiles (List.map points ~f:(fun (k, _, q) -> k, q)) in
    { kalshi_iqr
    ; model_iqr
    ; model_iqr_exact = Smile.quantile_above smile 0.25 -. Smile.quantile_above smile 0.75
    ; kalshi_median
    ; model_median
    }
  ;;
end
