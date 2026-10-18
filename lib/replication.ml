open! Core

module Model = struct
  type t =
    { spot0 : float
    ; horizon : float
    ; discount : float
    ; mean : float
    ; prob_above : float -> float
    ; partial_mean : float -> float
    }

  let create ~spot0 ~horizon ~discount ~mean ~prob_above ~partial_mean =
    { spot0; horizon; discount; mean; prob_above; partial_mean }
  ;;

  let of_density d =
    let points = Density.points d in
    let above k ~f =
      (* [>=] because payoff pieces include their left end: P(lo) - P(hi)
         is then exactly the probability of [lo, hi), atoms included. *)
      Array.sum (module Float) points ~f:(fun (s, p) -> if Float.(s >= k) then f s p else 0.)
    in
    create
      ~spot0:(Density.spot0 d)
      ~horizon:(Density.horizon d)
      ~discount:(Density.discount d)
      ~mean:(Density.expectation d ~f:Fn.id)
      ~prob_above:(fun k -> above k ~f:(fun _ p -> p))
      ~partial_mean:(fun k -> above k ~f:(fun s p -> s *. p))
  ;;

  let of_smile ?relative_step s =
    create
      ~spot0:(Smile.spot s)
      ~horizon:(Smile.years s)
      ~discount:(Smile.discount s)
      ~mean:(Smile.forward s)
      ~prob_above:(Smile.prob_above ?relative_step s)
      ~partial_mean:(Smile.partial_mean ?relative_step s)
  ;;

  let normal_cdf x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.)

  let lognormal ~spot0 ~horizon ~discount ~forward ~sigma =
    if not Float.(forward > 0. && sigma > 0. && horizon > 0.)
    then
      raise_s
        [%message
          "Model.lognormal: forward, sigma and horizon must be positive"
            (forward : float)
            (sigma : float)
            (horizon : float)];
    let v = sigma *. Float.sqrt horizon in
    let d k = (Float.log (forward /. k) +. (v *. v /. 2.)) /. v in
    (* With ln S_T normal: P(S_T >= K) = N(d2) and
       E[S_T 1{S_T >= K}] = F N(d1), where d2 = d1 - v. *)
    create
      ~spot0
      ~horizon
      ~discount
      ~mean:forward
      ~prob_above:(fun k -> normal_cdf (d k -. v))
      ~partial_mean:(fun k -> forward *. normal_cdf (d k))
  ;;

  (* The two curves, with their values at the ends of (0, infinity) filled
     in: everything is above 0, nothing is above infinity. *)
  let p t k =
    if Float.(k <= 0.) then 1. else if Float.is_finite k then t.prob_above k else 0.
  ;;

  let m t k =
    if Float.(k <= 0.) then t.mean else if Float.is_finite k then t.partial_mean k else 0.
  ;;

  let spot0 t = t.spot0
  let horizon t = t.horizon
  let discount t = t.discount
  let mean t = t.mean
  let prob_above = p
  let partial_mean = m
end

let integrate (model : Model.t) (f : Payoff.t) =
  model.discount
  *. List.sum
       (module Float)
       f
       ~f:(fun { lo; hi; a; b } ->
         (* Skip zero coefficients: a piece paying 0 contributes nothing,
            and skipping it avoids evaluating the curves far in the tails. *)
         let digital_part =
           if Float.equal a 0. then 0. else a *. (Model.p model lo -. Model.p model hi)
         and call_part =
           if Float.equal b 0. then 0. else b *. (Model.m model lo -. Model.m model hi)
         in
         digital_part +. call_part)
;;

(* Today's level mirrors [Pricer.value] at [Today]: the same rules for
   Give, And, Or, Scale and Cond, and the same date rules. Only the step
   into the horizon differs: compile the payoff and integrate it exactly,
   instead of evaluating it at every grid point. *)
let price (model : Model.t) c =
  let open Result.Let_syntax in
  let unpriceable e = Error (Payoff.Error.Unpriceable e) in
  let spot = model.spot0 in
  let rec value (c : Contract.t) =
    match c with
    | Zero -> return 0.
    | One -> return 1.
    | Give c -> value c >>| Float.neg
    | And (x, y) ->
      let%map x = value x
      and y = value y in
      x +. y
    | Or (x, y) ->
      let%map x = value x
      and y = value y in
      Float.max x y
    | Scale (o, c) ->
      let%map v = value c in
      Contract.Obs.eval o ~spot *. v
    | Cond (p, x, y) -> if Contract.Pred.eval p ~spot then value x else value y
    | At (t, c) ->
      (match Unpriceable.at_rule ~now:0. ~horizon:model.horizon t with
       | `Error e -> unpriceable e
       | `Now -> value c
       | `Horizon ->
         let%map payoff = Payoff.of_contract ~horizon:model.horizon c in
         integrate model payoff)
    | Anytime (t, c) ->
      (match Unpriceable.anytime_rule ~now:0. ~horizon:model.horizon t with
       | `Error e -> unpriceable e
       | `Now -> value c)
  in
  value c
;;

let price_exn model c =
  match price model c with
  | Ok v -> v
  | Error e -> raise_s [%message "unpriceable contract" (e : Payoff.Error.t)]
;;
