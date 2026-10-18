(* Made-up inputs for tests that must not touch the export window's
   quotes. Nothing here comes from data. *)

open! Core
open Contracts

(* One day to settlement, BTC-like levels, a 50%-ish at-the-money vol and a
   downward skew (rho < 0: puts dearer than calls). *)
let smile
  ?(forward = 100_000.)
  ?(years = 1. /. 365.)
  ?(params = { Smile.a = 0.0005; b = 0.004; rho = -0.3; m = 0.; s = 0.05 })
  ()
  =
  Smile.create ~spot:(forward -. 10.) ~forward ~years ~k_min:(-0.15) ~k_max:0.15 params
  |> Or_error.ok_exn
;;

(* Synthetic Kalshi quotes, made from a known "truth" so a test can check
   the analysis recovers what was planted.

   For each strike, [truth] gives the probability Kalshi's traders act on.
   The quote is that probability less and plus [half_spread], rounded out
   to whole cents (bid down, ask up) as Kalshi's 1-cent tick forces; a bid
   that would be under 1 cent is no bid, and asks are capped at 99 cents. *)
let quote ~truth ~half_spread strike =
  let p = Replication.Model.prob_above truth strike in
  let cents x = x *. 100. in
  let bid = Float.round_down (cents (p -. half_spread)) /. 100.
  and ask = Float.round_up (cents (p +. half_spread)) /. 100. in
  ( Option.some_if Float.(bid >= 0.01) bid
  , Some (Float.min ask 0.99) )
;;

type market =
  { kind : Export.Kind.t
  ; snapshots : int
  ; years : float
  ; strike_step : float
  ; strikes_each_side : int
  }

let daily = { kind = Daily; snapshots = 20; years = 1. /. 365.; strike_step = 250.; strikes_each_side = 16 }

let hourly =
  { kind = Hourly; snapshots = 20; years = 2. /. 8760.; strike_step = 100.; strikes_each_side = 15 }
;;

(* [snapshots] smiles, each a seeded random perturbation of the base smile
   (level of variance, skew, forward), with a ladder of strikes around the
   forward and quotes from [kalshi smile], the truth Kalshi trades on for
   that smile. Returns the export's own types, so [Report] runs on it
   unchanged. *)
let pairs ?(seed = 7) ?(half_spread = 0.01) market ~kalshi =
  let rng = Random.State.make [| seed |] in
  let uniform lo hi = lo +. Random.State.float rng (hi -. lo) in
  List.concat_map (List.range 0 market.snapshots) ~f:(fun i ->
    let forward = 100_000. +. uniform (-3000.) 3000. in
    (* Variance per year held near the base smile's, so both kinds have
       vols near 50%, and per-snapshot noise in level and skew. *)
    let scale = market.years *. 365. *. uniform 0.7 1.4 in
    let params =
      { Smile.a = 0.0005 *. scale
      ; b = 0.004 *. scale
      ; rho = uniform (-0.5) 0.
      ; m = 0.
      ; s = 0.05 *. Float.sqrt (market.years *. 365.)
      }
    in
    let smile = smile ~forward ~years:market.years ~params () in
    let truth = kalshi smile in
    let tag = Sexp.to_string (Export.Kind.sexp_of_t market.kind) in
    let snap_utc = sprintf "SYN-%s-%02d" tag i
    and event = sprintf "SYN-%s-EVENT-%02d" tag i in
    let surface =
      { Export.Surface.snap_utc
      ; event
      ; close_utc = "synthetic"
      ; kind = market.kind
      ; hours_to_close = market.years *. 8760.
      ; smile
      }
    in
    let centre = Float.round_nearest (forward /. market.strike_step) *. market.strike_step in
    List.map
      (List.range (-market.strikes_each_side) (market.strikes_each_side + 1))
      ~f:(fun j ->
        let strike = centre +. (Float.of_int j *. market.strike_step) in
        let bid, ask = quote ~truth ~half_spread strike in
        ( surface
        , { Export.Row.snap_utc
          ; event
          ; ticker = sprintf "%s-T%.0f" event strike
          ; strike
          ; bid
          ; ask
          ; bid_size = Some 1000.
          ; ask_size = Some 1000.
          ; in_range = Smile.in_quoted_range smile strike
          ; p_opt = Smile.prob_above smile strike
          } )))
;;
