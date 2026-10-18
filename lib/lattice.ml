open! Core

type t =
  { n : int
  ; horizon : float
  ; dt : float
  ; spot0 : float
  ; discount : float
  ; df_step : float (* discount factor per step *)
  ; prices : float array array (* prices.(m).(j); j counts up moves *)
  ; up : float array array (* up.(m).(j): probability of (m, j) -> (m+1, j+1) *)
  ; reachable : bool array array (* false where no path with positive probability goes *)
  ; node_prob : float array array (* probability of being at each node *)
  ; terminal_q : float array
  }

(* log (exp a + exp b), without overflow, and with -infinity meaning
   "probability zero". *)
let log_add a b =
  if Float.(a = neg_infinity)
  then b
  else if Float.(b = neg_infinity)
  then a
  else (
    let hi = Float.max a b
    and lo = Float.min a b in
    hi +. Float.log1p (Float.exp (lo -. hi)))
;;

(* The strike K with P(S_T >= K) = p, by bisection in log-strike. *)
let quantile model p =
  let mean = Replication.Model.mean model in
  let rec go lo hi n =
    if n = 0
    then Float.exp ((lo +. hi) /. 2.)
    else (
      let mid = (lo +. hi) /. 2. in
      if Float.(Replication.Model.prob_above model (Float.exp mid) > p)
      then go mid hi (n - 1)
      else go lo mid (n - 1))
  in
  go (Float.log mean -. 15.) (Float.log mean +. 15.) 200
;;

let create ~steps:n model =
  if n < 1
  then Or_error.error_s [%message "a tree needs at least one step" (n : int)]
  else (
    let p = Replication.Model.prob_above model
    and m = Replication.Model.partial_mean model in
    let horizon = Replication.Model.horizon model in
    (* Centre and spread of log S_T from the median and quartiles, which
       exist for any distribution. 0.6745 is the normal quartile, so for a
       normal distribution this is its standard deviation. *)
    let centre = Float.log (quantile model 0.5) in
    let spread =
      (Float.log (quantile model 0.25) -. Float.log (quantile model 0.75)) /. (2. *. 0.6745)
    in
    let x j =
      centre +. (spread *. Float.of_int ((2 * j) - n) /. Float.sqrt (Float.of_int n))
    in
    (* Final node j collects the probability between the midpoints to its
       neighbours; the end nodes take the tails. *)
    let edge j =
      if j = 0
      then 0.
      else if j = n + 1
      then Float.infinity
      else Float.exp ((x (j - 1) +. x j) /. 2.)
    in
    let raw =
      Array.init (n + 1) ~f:(fun j ->
        let lo = edge j
        and hi = edge (j + 1) in
        (* A smile's finite-difference probabilities can be a hair
           non-monotone; negative mass is treated as none. *)
        Float.max 0. (p lo -. p hi), m lo -. m hi)
    in
    let total = Array.sum (module Float) raw ~f:fst in
    let terminal_q = Array.map raw ~f:(fun (q, _) -> q /. total) in
    (* Each node's price is the average price in its bin, so that
       sum_j q_j S_j equals the distribution's mean exactly. *)
    let terminal_s =
      Array.mapi raw ~f:(fun j (q, mass) ->
        if Float.(q > 1e-300) then mass /. q else Float.exp (x j))
    in
    (* Rubinstein: every path to final node j has probability
       q_j / C(n, j). Work in logs: C(n, j) overflows long before the
       probabilities stop mattering. *)
    let log_choose =
      Array.folding_map (Array.init (n + 1) ~f:Fn.id) ~init:0. ~f:(fun acc j ->
        let acc =
          if j = 0 then 0. else acc +. Float.log (Float.of_int (n - j + 1)) -. Float.log (Float.of_int j)
        in
        acc, acc)
    in
    let log_path = Array.make_matrix ~dimx:(n + 1) ~dimy:0 0. in
    log_path.(n)
    <- Array.mapi terminal_q ~f:(fun j q ->
         if Float.(q > 0.) then Float.log q -. log_choose.(j) else Float.neg_infinity);
    let prices = Array.make_matrix ~dimx:(n + 1) ~dimy:0 0. in
    let up = Array.make_matrix ~dimx:(n + 1) ~dimy:0 0. in
    prices.(n) <- terminal_s;
    (* Growth per step that brings the mean back to today's spot. Under a
       pure interest rate this is 1 / discount per step, so discounted
       prices are fair; with Deribit's zero rate and a forward above spot,
       it carries the spot-forward basis. *)
    let growth =
      (Replication.Model.mean model /. Replication.Model.spot0 model)
      ** (1. /. Float.of_int n)
    in
    for s = n - 1 downto 0 do
      let next = log_path.(s + 1) in
      log_path.(s) <- Array.init (s + 1) ~f:(fun j -> log_add next.(j) next.(j + 1));
      up.(s)
      <- Array.init (s + 1) ~f:(fun j ->
           if Float.(log_path.(s).(j) = neg_infinity)
           then 0.5
           else Float.exp (next.(j + 1) -. log_path.(s).(j)));
      prices.(s)
      <- Array.init (s + 1) ~f:(fun j ->
           let u = up.(s).(j) in
           ((u *. prices.(s + 1).(j + 1)) +. ((1. -. u) *. prices.(s + 1).(j))) /. growth)
    done;
    let reachable =
      Array.map log_path ~f:(Array.map ~f:(fun l -> Float.(l > neg_infinity)))
    in
    (* Probability of each node, pushed forward from today with the up
       probabilities. *)
    let node_prob = Array.init (n + 1) ~f:(fun s -> Array.create ~len:(s + 1) 0.) in
    node_prob.(0).(0) <- 1.;
    for s = 0 to n - 1 do
      for j = 0 to s do
        let u = up.(s).(j)
        and here = node_prob.(s).(j) in
        node_prob.(s + 1).(j + 1) <- node_prob.(s + 1).(j + 1) +. (here *. u);
        node_prob.(s + 1).(j) <- node_prob.(s + 1).(j) +. (here *. (1. -. u))
      done
    done;
    let discount = Replication.Model.discount model in
    Ok
      { n
      ; horizon
      ; dt = horizon /. Float.of_int n
      ; spot0 = Replication.Model.spot0 model
      ; discount
      ; df_step = discount ** (1. /. Float.of_int n)
      ; prices
      ; up
      ; reachable
      ; node_prob
      ; terminal_q
      })
;;

let steps t = t.n
let dt t = t.dt
let node_price t ~step ~node = t.prices.(step).(node)
let up_probability t ~step ~node = t.up.(step).(node)
let node_probability t ~step ~node = t.node_prob.(step).(node)

let terminal_density t =
  let rate = -.Float.log t.discount /. t.horizon in
  Array.to_list (Array.zip_exn t.prices.(t.n) t.terminal_q)
  |> List.filter ~f:(fun (_, q) -> Float.(q > 0.))
  |> Density.create ~spot0:t.spot0 ~horizon:t.horizon ~rate
  |> Or_error.ok_exn
;;

(* Which step a date falls on, judged from step [now]. *)
let step_of t date ~now : [ `Step of int | `Error of Unpriceable.t ] =
  let now_time = Float.of_int now *. t.dt in
  if Unpriceable.same_time date now_time
  then `Step now
  else if Float.(date < now_time)
  then `Error (Already_passed { date; now = now_time })
  else if Float.(date > t.horizon) && not (Unpriceable.same_time date t.horizon)
  then `Error (Unsupported_date { requested = date; model_horizon = t.horizon })
  else (
    let k = Float.iround_nearest_exn (date /. t.dt) in
    if Unpriceable.same_time (Float.of_int k *. t.dt) date
    then `Step k
    else `Error (Off_grid { requested = date; step = t.dt }))
;;

(* Values one step earlier: each node's discounted expected value. *)
let back_one t ~step next =
  Array.init (step + 1) ~f:(fun j ->
    let u = t.up.(step).(j) in
    t.df_step *. ((u *. next.(j + 1)) +. ((1. -. u) *. next.(j))))
;;

let evaluator t =
  (* [eval c m] is the value of acquiring [c] at each node of step [m].
     It is memoised on (step, contract): an [At] or [Anytime] at step m is
     built from the same contract at step m + 1, so each subcontract is
     evaluated once per step and the whole tree costs O(steps^2). *)
  let memo = Hashtbl.Poly.create () in
  let rec eval (c : Contract.t) m =
    match Hashtbl.find memo (m, c) with
    | Some r -> r
    | None ->
      let r = eval_uncached c m in
      Hashtbl.set memo ~key:(m, c) ~data:r;
      r
  and eval_uncached (c : Contract.t) m : (float array, Unpriceable.t) Result.t =
    let open Result.Let_syntax in
    let spots = t.prices.(m) in
    let width = m + 1 in
    match c with
    | Zero -> return (Array.create ~len:width 0.)
    | One -> return (Array.create ~len:width 1.)
    | Give c -> eval c m >>| Array.map ~f:Float.neg
    | And (a, b) ->
      let%map a = eval a m
      and b = eval b m in
      Array.map2_exn a b ~f:( +. )
    | Or (a, b) ->
      (* Choose at this node, knowing the price here. *)
      let%map a = eval a m
      and b = eval b m in
      Array.map2_exn a b ~f:Float.max
    | Scale (o, c) ->
      let%map v = eval c m in
      Array.mapi v ~f:(fun j x -> Contract.Obs.eval o ~spot:spots.(j) *. x)
    | Cond (p, a, b) ->
      let holds = Array.map spots ~f:(fun spot -> Contract.Pred.eval p ~spot) in
      (* Only a branch some reachable node takes can make this unpriceable. *)
      let needed want =
        Array.existsi holds ~f:(fun j h -> Bool.equal h want && t.reachable.(m).(j))
      in
      let branch want c =
        if needed want then eval c m else Ok (Array.create ~len:width 0.)
      in
      let%map a = branch true a
      and b = branch false b in
      Array.init width ~f:(fun j -> if holds.(j) then a.(j) else b.(j))
    | At (date, inner) ->
      (match step_of t date ~now:m with
       | `Error e -> Error e
       | `Step k when k = m -> eval inner m
       | `Step _ ->
         let%map next = eval c (m + 1) in
         back_one t ~step:m next)
    | Anytime (date, inner) ->
      (match step_of t date ~now:m with
       | `Error e -> Error e
       | `Step k when k = m -> eval inner m
       | `Step _ ->
         (* Take the better of acquiring now and keeping the right open
            one more step. At the deadline itself, acquiring is compulsory
            (the case above). *)
         let%map now = eval inner m
         and later = eval c (m + 1) in
         Array.map2_exn now (back_one t ~step:m later) ~f:Float.max)
  in
  eval
;;

let values t c =
  (* One evaluator for every step, so its memo table is shared: the whole
     table costs what pricing does, O(steps^2). *)
  let eval = evaluator t in
  List.init (t.n + 1) ~f:(eval c) |> Result.all |> Result.map ~f:Array.of_list
;;

let price t c = Result.map (evaluator t c 0) ~f:(fun v -> v.(0))

let price_exn t c =
  match price t c with
  | Ok v -> v
  | Error e -> raise_s [%message "unpriceable contract" (e : Unpriceable.t)]
;;
