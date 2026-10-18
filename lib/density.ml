open! Core

type t =
  { spot0 : float
  ; horizon : float
  ; rate : float
  ; points : (float * float) array (* sorted by price; probabilities sum to 1 *)
  }
[@@deriving sexp_of]

let create ?(tolerance = 1e-6) ~spot0 ~horizon ~rate points =
  let total = List.sum (module Float) points ~f:snd in
  (* [Float.is_finite] rules out NaN and infinity. NaN fails every
     comparison, so without this check "p < 0." would quietly let it in. *)
  if List.is_empty points
  then Or_error.error_s [%message "density has no points"]
  else if List.exists points ~f:(fun (s, p) ->
            not (Float.is_finite s && Float.is_finite p))
  then Or_error.error_s [%message "non-finite price or probability in density"]
  else if List.exists points ~f:(fun (_, p) -> Float.(p < 0.))
  then Or_error.error_s [%message "negative probability in density"]
  else if Float.(abs (total - 1.) > tolerance)
  then
    Or_error.error_s
      [%message "probabilities do not sum to 1" (total : float) (tolerance : float)]
  else (
    let points =
      points
      |> List.map ~f:(fun (s, p) -> s, p /. total)
      |> List.sort ~compare:(fun (a, _) (b, _) -> Float.compare a b)
      |> Array.of_list
    in
    Ok { spot0; horizon; rate; points })
;;

(* Accept only plain decimal or scientific notation. [Float.of_string] alone
   also accepts "0x5A" (hex) and "1_00" (OCaml's digit separator), which in
   a data file are almost certainly mistakes. *)
let parse_number s =
  if (not (String.is_empty s))
     && String.for_all s ~f:(fun c -> Char.is_digit c || String.mem ".+-eE" c)
  then Float.of_string_opt s
  else None
;;

(* One line of a CSV becomes [None] (skip it), [Some (Ok point)], or
   [Some (Error _)]. Keeping "skip" separate from "error" means comments and
   blank lines never count as failures. *)
let parse_line ~line_number line =
  (* Strip a UTF-8 byte-order mark, which some editors put at the start of a
     file, and whitespace including Windows' trailing \r. *)
  let line = String.chop_prefix_if_exists line ~prefix:"\xEF\xBB\xBF" |> String.strip in
  let fields = String.split line ~on:',' |> List.map ~f:String.strip in
  let is_header =
    List.equal String.equal (List.map fields ~f:String.lowercase) [ "price"; "probability" ]
  in
  if String.is_empty line || String.is_prefix line ~prefix:"#" || is_header
  then None
  else (
    let bad reason =
      Some (Or_error.error_s [%message reason (line_number : int) (line : string)])
    in
    match fields with
    | [ s; p ] ->
      (match parse_number s, parse_number p with
       | Some s, Some p when Float.(s > 0.) && Float.(p >= 0.) -> Some (Ok (s, p))
       | Some _, Some _ -> bad "price must be positive and probability non-negative"
       | _ -> bad "expected two numbers")
    | _ -> bad "expected two comma-separated fields")
;;

let of_csv_lines ?tolerance ~spot0 ~horizon ~rate lines =
  let open Or_error.Let_syntax in
  (* [Or_error.all] collects every bad line, not just the first, so one run
     shows all the problems in a file. *)
  let%bind points =
    List.filter_mapi lines ~f:(fun i line -> parse_line ~line_number:(i + 1) line)
    |> Or_error.all
  in
  create ?tolerance ~spot0 ~horizon ~rate points
;;

let of_csv ?tolerance ~spot0 ~horizon ~rate filename =
  match In_channel.read_lines filename with
  | lines -> of_csv_lines ?tolerance ~spot0 ~horizon ~rate lines
  | exception exn -> Or_error.of_exn exn
;;

(* Standard normal CDF. Core doesn't expose erf, so we borrow OCaml's
   standard library version. *)
let normal_cdf z = 0.5 *. (1. +. Stdlib.Float.erf (z /. Float.sqrt 2.))

(* Under Black-Scholes, ln S_T ~ Normal(m, s^2) with
     m = ln S0 + (r - vol^2/2) T,   s = vol * sqrt T.
   We cut log-price space into [n] equal bins spanning m +/- width_sd * s.
   Each bin's probability is a difference of normal CDFs. Each bin's price
   is the *conditional mean* of S_T inside the bin rather than its midpoint:
   that choice makes E[S_T] come out exactly right (= S0 e^{rT}), so
   forwards price exactly even on a coarse grid. *)
let lognormal ?(n = 4001) ?(width_sd = 8.) ~spot0 ~horizon ~rate ~vol () =
  let m = Float.log spot0 +. ((rate -. (vol *. vol /. 2.)) *. horizon) in
  let s = vol *. Float.sqrt horizon in
  let lo = m -. (width_sd *. s) in
  let step = 2. *. width_sd *. s /. Float.of_int n in
  let mean_factor = Float.exp (m +. (s *. s /. 2.)) in
  let points =
    List.init n ~f:(fun i ->
      let a = lo +. (Float.of_int i *. step) in
      let b = a +. step in
      let za = (a -. m) /. s
      and zb = (b -. m) /. s in
      let prob = normal_cdf zb -. normal_cdf za in
      let partial_mean = mean_factor *. (normal_cdf (zb -. s) -. normal_cdf (za -. s)) in
      let price =
        if Float.(prob > 1e-300)
        then partial_mean /. prob
        else Float.exp ((a +. b) /. 2.)
      in
      price, prob)
  in
  create ~spot0 ~horizon ~rate points |> Or_error.ok_exn
;;

let spot0 t = t.spot0
let horizon t = t.horizon
let rate t = t.rate
(* A copy, so callers can't mutate the validated array behind our back. *)
let points t = Array.copy t.points
let discount t = Float.exp (-.t.rate *. t.horizon)

let expectation t ~f =
  Array.sum (module Float) t.points ~f:(fun (s, p) -> p *. f s)
;;
