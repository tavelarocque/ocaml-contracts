open! Core

module Kind = struct
  type t =
    | Hourly
    | Daily
  [@@deriving sexp, equal]

  let of_string = function
    | "hourly" -> Ok Hourly
    | "daily" -> Ok Daily
    | other -> Or_error.error_s [%message "unknown kind" (other : string)]
  ;;
end

module Surface = struct
  type t =
    { snap_utc : string
    ; event : string
    ; close_utc : string
    ; kind : Kind.t
    ; hours_to_close : float
    ; smile : Smile.t
    }
  [@@deriving sexp_of]
end

module Row = struct
  type t =
    { snap_utc : string
    ; event : string
    ; ticker : string
    ; strike : float
    ; bid : float option
    ; ask : float option
    ; bid_size : float option
    ; ask_size : float option
    ; in_range : bool
    ; p_opt : float
    }
  [@@deriving sexp_of]
end

(* A tiny CSV reader for these files: no quoted fields, so splitting on
   commas is enough. [read file ~f] calls [f get] for every data row, where
   [get "column"] returns that row's field, and collects every error
   (with its line number) rather than stopping at the first. *)
let read file ~f =
  let open Or_error.Let_syntax in
  let%bind lines = Or_error.try_with (fun () -> In_channel.read_lines file) in
  match lines with
  | [] -> Or_error.error_s [%message "empty file" (file : string)]
  | header :: rows ->
    let columns =
      String.split (String.strip header) ~on:','
      |> List.mapi ~f:(fun i name -> name, i)
      |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
    in
    List.filter_mapi rows ~f:(fun i line ->
      let line = String.strip line in
      if String.is_empty line
      then None
      else (
        let fields = String.split line ~on:',' |> Array.of_list in
        let get name =
          match Map.find columns name with
          | None -> Or_error.error_s [%message "missing column" (name : string)]
          | Some j when j >= Array.length fields ->
            Or_error.error_s [%message "row too short" (name : string)]
          | Some j -> Ok fields.(j)
        in
        Some
          (f get
           |> Or_error.tag_s ~tag:[%message (file : string) ~line:(i + 2 : int)])))
    |> Or_error.all
;;

let float_field get name =
  let open Or_error.Let_syntax in
  let%bind s = get name in
  match Float.of_string_opt s with
  | Some x when Float.is_finite x -> Ok x
  | _ -> Or_error.error_s [%message "not a number" (name : string) (s : string)]
;;

let optional_float_field get name =
  let open Or_error.Let_syntax in
  let%bind s = get name in
  if String.is_empty s then Ok None else float_field get name |> Or_error.map ~f:Option.some
;;

let load_surfaces file =
  read file ~f:(fun get ->
    let open Or_error.Let_syntax in
    let num = float_field get in
    let%bind snap_utc = get "snap_utc"
    and event = get "event"
    and close_utc = get "close_utc"
    and kind = get "kind" >>= Kind.of_string
    and hours_to_close = num "hours_to_close"
    and spot = num "spot"
    and forward = num "F"
    and years = num "T"
    and a = num "a"
    and b = num "b"
    and rho = num "rho"
    and m = num "m"
    and s = num "s"
    and k_min = num "k_min"
    and k_max = num "k_max" in
    let%map smile =
      Smile.create ~spot ~forward ~years ~k_min ~k_max { Smile.a; b; rho; m; s }
    in
    { Surface.snap_utc; event; close_utc; kind; hours_to_close; smile })
;;

let load_rows file =
  read file ~f:(fun get ->
    let open Or_error.Let_syntax in
    let%map snap_utc = get "snap_utc"
    and event = get "event"
    and ticker = get "ticker"
    and strike = float_field get "strike"
    and bid = optional_float_field get "bid"
    and ask = optional_float_field get "ask"
    and bid_size = optional_float_field get "bid_size"
    and ask_size = optional_float_field get "ask_size"
    and in_range =
      get "in_range"
      >>= function
      | "1" -> Ok true
      | "0" -> Ok false
      | other -> Or_error.error_s [%message "in_range must be 0 or 1" (other : string)]
    and p_opt = float_field get "p_opt" in
    { Row.snap_utc; event; ticker; strike; bid; ask; bid_size; ask_size; in_range; p_opt })
;;

let join surfaces rows =
  let open Or_error.Let_syntax in
  let key snap event = snap ^ "|" ^ event in
  let%bind () =
    match
      List.find_all_dups surfaces ~compare:(fun (x : Surface.t) y ->
        String.compare (key x.snap_utc x.event) (key y.snap_utc y.event))
    with
    | [] -> Ok ()
    | (d : Surface.t) :: _ ->
      Or_error.error_s
        [%message "two smiles for the same snapshot and event" d.snap_utc d.event]
  in
  let by_key =
    List.map surfaces ~f:(fun (s : Surface.t) -> key s.snap_utc s.event, s)
    |> Hashtbl.of_alist_multi (module String)
  in
  List.map rows ~f:(fun (r : Row.t) ->
    match Hashtbl.find by_key (key r.snap_utc r.event) with
    | Some (s :: _) -> Ok (s, r)
    | Some [] | None ->
      Or_error.error_s
        [%message "no smile for contract" (r.ticker : string) (r.snap_utc : string)])
  |> Or_error.all
;;
