open! Core

module Unpriceable = Unpriceable

(* Where we are when evaluating a sub-contract.
   - [Today]: nothing has happened yet; Spot is today's price.
   - [Horizon s]: we are at the density's horizon and the price turned out
     to be [s]. Every observable is now known. *)
type stage =
  | Today
  | Horizon of float

(* The current time at each stage, in years from today. *)
let now model = function
  | Today -> 0.
  | Horizon _ -> Density.horizon model
;;

(* [value model stage c] is the worth of acquiring [c] at [stage].

   The recursion mirrors the type: one case per constructor, and the
   compiler will refuse to build this file if a constructor is missing.

   [Result.Let_syntax] with [let%map] / [let%bind] (from ppx_let) is how
   Jane Street code threads errors: if any sub-contract is unpriceable, the
   whole contract is, and the first error wins. *)
let rec value model stage (c : Contract.t) : (float, Unpriceable.t) Result.t =
  let open Result.Let_syntax in
  let spot =
    match stage with
    | Today -> Density.spot0 model
    | Horizon s -> s
  in
  match c with
  | Zero -> return 0.
  | One -> return 1.
  | Give c ->
    let%map v = value model stage c in
    -.v
  | And (a, b) ->
    let%map va = value model stage a
    and vb = value model stage b in
    va +. vb
  | Or (a, b) ->
    (* The holder picks the better one at the moment of acquisition. Today
       that means comparing today's values. At the horizon it means
       comparing payoffs once the price is known — which is why
       [At T (Or (S-K, 0))] is a call option, but [Or (call, put)] held
       today is just whichever of the two is pricier now. *)
    let%map va = value model stage a
    and vb = value model stage b in
    Float.max va vb
  | Scale (o, c) ->
    let%map v = value model stage c in
    Contract.Obs.eval o ~spot *. v
  | Cond (p, a, b) ->
    if Contract.Pred.eval p ~spot then value model stage a else value model stage b
  | At (t, c) ->
    (match Unpriceable.at_rule ~now:(now model stage) ~horizon:(Density.horizon model) t with
     | `Error e -> Error e
     | `Now -> value model stage c
     | `Horizon ->
       (* The heart of the model: value [c] in every possible horizon state,
          weight by probability, then discount back to today.
            price = e^{-rT} * sum_i p_i * value_at_horizon(s_i)
          This is Breeden-Litzenberger / Carr-Madan in one line. *)
       let%map weighted =
         Density.points model
         |> Array.to_list
         |> List.map ~f:(fun (s, p) ->
           let%map v = value model (Horizon s) c in
           p *. v)
         |> Result.all
       in
       Density.discount model *. List.sum (module Float) weighted ~f:Fn.id)
  | Anytime (t, c) ->
    (match Unpriceable.anytime_rule ~now:(now model stage) ~horizon:(Density.horizon model) t with
     | `Error e -> Error e
     | `Now -> value model stage c)
;;

let price model c = value model Today c

let price_exn model c =
  match price model c with
  | Ok v -> v
  | Error e -> raise_s [%message "unpriceable contract" (e : Unpriceable.t)]
;;
