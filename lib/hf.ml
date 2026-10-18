open! Core

(* Days since 1970-01-01 of a proleptic Gregorian date (Howard Hinnant's
   days_from_civil). *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) % 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468
;;

(* The inverse: civil_from_days. *)
let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  (if m <= 2 then y + 1 else y), m, d
;;

let to_utc_string x =
  let secs = Float.iround_down_exn x in
  let micros = Float.iround_nearest_exn ((x -. Float.of_int secs) *. 1e6) in
  let secs, micros = if micros >= 1_000_000 then secs + 1, 0 else secs, micros in
  let days = secs / 86400
  and rem = secs % 86400 in
  let days = if rem < 0 then days - 1 else days in
  let y, m, d = civil_from_days days in
  sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06d+00:00" y m d (rem / 3600) (rem % 3600 / 60) (rem % 60)
    micros
;;

let parse_utc s =
  try
    let s = String.strip s in
    let body, ok_zone =
      if String.is_suffix s ~suffix:"Z"
      then String.drop_suffix s 1, true
      else if String.is_suffix s ~suffix:"+00:00"
      then String.drop_suffix s 6, true
      else s, false
    in
    if not ok_zone
    then Or_error.error_s [%message "expected a UTC time ending in Z or +00:00" s]
    else (
      let int a b = Int.of_string (String.sub body ~pos:a ~len:b) in
      if String.length body < 19 || not (Char.equal body.[10] 'T')
      then Or_error.error_s [%message "not an ISO 8601 time" s]
      else (
        let frac =
          if String.length body > 19 then Float.of_string ("0" ^ String.drop_prefix body 19) else 0.
        in
        let days = days_from_civil (int 0 4) (int 5 2) (int 8 2) in
        Ok
          (Float.of_int ((days * 86400) + (int 11 2 * 3600) + (int 14 2 * 60) + int 17 2)
           +. frac)))
  with
  | exn -> Or_error.error_s [%message "bad time" s (exn : exn)]
;;

module Book = struct
  type t =
    { t : float
    ; event : string
    ; ticker : string
    ; strike : float
    ; close : float
    ; bid : float option
    ; ask : float option
    ; bid_size : float option
    ; ask_size : float option
    }
  [@@deriving sexp_of]
end

module Trade = struct
  type t =
    { t : float
    ; ticker : string
    ; price : float
    ; count : float
    ; taker_bought_yes : bool
    }
  [@@deriving sexp_of]
end

(* A small CSV reader: these exports never quote fields. Each row is read
   by column name, and every error carries its line number. *)
let read_csv path ~f =
  let open Or_error.Let_syntax in
  let%bind lines = Or_error.try_with (fun () -> In_channel.read_lines path) in
  match lines with
  | [] -> Or_error.error_s [%message "empty file" path]
  | header :: rows ->
    let names = String.split header ~on:',' |> List.map ~f:String.strip in
    let index = List.mapi names ~f:(fun i n -> n, i) |> String.Map.of_alist_exn in
    List.filter_mapi rows ~f:(fun i line ->
      if String.is_empty (String.strip line)
      then None
      else (
        let cells = String.split line ~on:',' |> Array.of_list in
        let get name =
          match Map.find index name with
          | None -> Or_error.error_s [%message "missing column" name]
          | Some j when j < Array.length cells -> Ok (String.strip cells.(j))
          | Some _ -> Or_error.error_s [%message "short row"]
        in
        Some (Or_error.tag_s (f get) ~tag:[%message path ~line:(i + 2 : int)])))
    |> Or_error.combine_errors
;;

let float_cell s = Or_error.try_with (fun () -> Float.of_string s)
let opt_float s = if String.is_empty s then Ok None else Or_error.map (float_cell s) ~f:Option.some

let load_books path =
  read_csv path ~f:(fun get ->
    let open Or_error.Let_syntax in
    let%bind t = get "t" >>= parse_utc
    and event = get "event"
    and ticker = get "ticker"
    and strike = get "strike" >>= float_cell
    and close = get "close_utc" >>= parse_utc
    and bid = get "bid" >>= opt_float
    and ask = get "ask" >>= opt_float
    and bid_size = get "bid_size" >>= opt_float
    and ask_size = get "ask_size" >>= opt_float in
    return { Book.t; event; ticker; strike; close; bid; ask; bid_size; ask_size })
;;

let load_trades path =
  read_csv path ~f:(fun get ->
    let open Or_error.Let_syntax in
    let%bind t = get "t" >>= parse_utc
    and ticker = get "ticker"
    and price = get "price" >>= float_cell
    and count = get "count" >>= float_cell
    and side = get "taker_side" in
    let%bind taker_bought_yes =
      match side with
      | "yes" -> Ok true
      | "no" -> Ok false
      | other -> Or_error.error_s [%message "taker_side must be yes or no" other]
    in
    return { Trade.t; ticker; price; count; taker_bought_yes })
;;

let load_index path =
  let open Or_error.Let_syntax in
  let%map rows =
    read_csv path ~f:(fun get ->
      let%bind t = get "t" >>= parse_utc
      and x = get "index" >>= float_cell in
      return (t, x))
  in
  Array.of_list rows |> Array.sorted_copy ~compare:(fun (a, _) (b, _) -> Float.compare a b)
;;
