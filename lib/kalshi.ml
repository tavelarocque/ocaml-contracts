(* Kalshi crypto contracts expressed in the language.

   A YES contract pays $1 if the event happens, so every Kalshi crypto
   market here is a digital or a range from [Instruments]. Kalshi's own
   conventions (exact settlement index, inclusive/exclusive edges, fees)
   are deliberately NOT modelled yet — that is Milestone 2 work. *)

open! Core

(* "BTC at or above [strike] at [expiry]". *)
let above ~expiry ~strike = Instruments.digital_above ~expiry ~strike

(* "BTC below [strike] at [expiry]". Together with [above] at the same strike
   it covers every outcome exactly once, so the pair is worth exactly $1 at
   expiry whatever the model says — a free consistency check. *)
let below ~expiry ~strike = Instruments.digital_below ~expiry ~strike

(* A full ladder of mutually exclusive buckets covering every price:
   below the first edge, each [edge_i, edge_{i+1}), and at-or-above the last.
   Exactly one bucket pays $1, so the ladder's total price must equal the
   price of $1 at expiry — a property the tests check. *)
let ladder ~expiry ~edges =
  match edges, List.last edges with
  | [], _ | _, None -> []
  | first :: _, Some last ->
    let num = Contract.number_to_string in
    let low = "below " ^ num first, Instruments.digital_below ~expiry ~strike:first in
    let high = num last ^ " or above", Instruments.digital_above ~expiry ~strike:last in
    let middle =
      List.zip_exn (List.drop_last_exn edges) (List.tl_exn edges)
      |> List.map ~f:(fun (lo, hi) ->
        sprintf "%s to %s" (num lo) (num hi), Instruments.range ~expiry ~lo ~hi)
    in
    (low :: middle) @ [ high ]
;;

(* Edges [count] steps either side of the multiple of [step] nearest
   [centre]: the strikes of a ladder around the forward. *)
let edges_around ~centre ~step ~each_side =
  let mid = Float.round_nearest (centre /. step) *. step in
  List.map (List.range (-each_side) (each_side + 1)) ~f:(fun j ->
    mid +. (Float.of_int j *. step))
;;
