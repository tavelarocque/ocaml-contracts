(* A synthetic high-frequency market with a known answer, for the markout
   tests. Nothing here comes from data.

   BTC follows a lognormal random walk second by second over 12 hours, with
   no drift in price (a martingale, as the risk-neutral measure requires)
   and a 50% vol. Eight hourly events close at hours 5 to 12, each with a
   flat "smile" at that vol every hour for the four hours before its close,
   so the options-implied fair value of "above K" is exactly Black's N(d2),
   which [true_value] writes out independently of the code under test.

   Trades arrive at random in each event from four hours to fifteen minutes
   before its close. Each is uninformed (random side, paying a 2-cent
   half-spread around fair value) or, with probability [informed], placed
   by a trader who sees fair value [lookahead] seconds ahead and trades
   only when that beats the half-spread. *)

open! Core
open Contracts

let t0 = Hf.parse_utc "2030-01-01T00:00:00Z" |> Or_error.ok_exn
let sigma = 0.5
let year = 365. *. 86400.
let half_spread = 0.02
let hours = 12

type event =
  { name : string
  ; close : float
  }

let events =
  List.init 8 ~f:(fun k ->
    { name = sprintf "SYN-EVENT-%d" (k + 5); close = t0 +. (3600. *. Float.of_int (k + 5)) })
;;

let normal_cdf x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.)

let index_path ~seed =
  let rng = Random.State.make [| seed |] in
  let gaussian () =
    (* Box-Muller *)
    let u = Float.max 1e-300 (Random.State.float rng 1.)
    and v = Random.State.float rng 1. in
    Float.sqrt (-2. *. Float.log u) *. Float.cos (2. *. Float.pi *. v)
  in
  let dt = 1. /. year in
  let x = ref 100_000. in
  Array.init ((hours * 3600) + 1) ~f:(fun i ->
    let here = !x in
    x := here *. Float.exp ((sigma *. Float.sqrt dt *. gaussian ()) -. (sigma *. sigma *. dt /. 2.));
    t0 +. Float.of_int i, here)
;;

let index_at index at = snd index.(Float.iround_nearest_exn (at -. t0))

let true_value index ~close ~strike ~at =
  let x = index_at index at in
  if Float.(at >= close)
  then if Float.(x > strike) then 1. else 0.
  else (
    let v = sigma *. Float.sqrt ((close -. at) /. year) in
    normal_cdf ((Float.log (x /. strike) -. (v *. v /. 2.)) /. v))
;;

let surfaces index =
  List.concat_map events ~f:(fun e ->
    List.init 4 ~f:(fun j ->
      let t = e.close -. (3600. *. Float.of_int (4 - j)) in
      let x = index_at index t in
      let years = (e.close -. t) /. year in
      let smile =
        Smile.create ~spot:x ~forward:x ~years ~k_min:(-1.) ~k_max:1.
          { a = sigma *. sigma *. years; b = 0.; rho = 0.; m = 0.; s = 0. }
        |> Or_error.ok_exn
      in
      { Export.Surface.snap_utc = Hf.to_utc_string t
      ; event = e.name
      ; close_utc = Hf.to_utc_string e.close
      ; kind = Hourly
      ; hours_to_close = (e.close -. t) /. 3600.
      ; smile
      }))
;;

let strikes = List.init 9 ~f:(fun j -> 100_000. +. (250. *. Float.of_int (j - 4)))
let ticker e k = sprintf "%s-T%.0f" e.name k

let books =
  List.concat_map events ~f:(fun e ->
    List.map strikes ~f:(fun strike ->
      { Hf.Book.t = t0
      ; event = e.name
      ; ticker = ticker e strike
      ; strike
      ; close = e.close
      ; bid = None
      ; ask = None
      ; bid_size = None
      ; ask_size = None
      }))
;;

let trades index ~informed ~lookahead ~seed =
  let rng = Random.State.make [| seed |] in
  let strikes = Array.of_list strikes in
  List.concat_map events ~f:(fun e ->
    let first = Float.iround_nearest_exn (e.close -. t0) - (4 * 3600)
    and last = Float.iround_nearest_exn (e.close -. t0) - (15 * 60) in
    List.filter_map (List.range first last) ~f:(fun s ->
      if Float.(Random.State.float rng 1. >= 0.04)
      then None
      else (
        let at = t0 +. Float.of_int s in
        let strike = strikes.(Random.State.int rng (Array.length strikes)) in
        let v = true_value index ~close:e.close ~strike ~at in
        if Float.(v < 0.05 || v > 0.95)
        then None
        else (
          let buy_yes =
            if Float.(Random.State.float rng 1. < informed)
            then (
              let future = true_value index ~close:e.close ~strike ~at:(at +. lookahead) in
              if Float.(future > v +. half_spread)
              then Some true
              else if Float.(future < v -. half_spread)
              then Some false
              else None)
            else Some (Random.State.bool rng)
          in
          Option.map buy_yes ~f:(fun b ->
            { Hf.Trade.t = at
            ; ticker = ticker e strike
            ; price = (if b then v +. half_spread else v -. half_spread)
            ; count = 1.
            ; taker_bought_yes = b
            })))))
;;
