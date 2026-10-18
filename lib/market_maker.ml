open! Core

module Params = struct
  type t =
    { gamma : float
    ; k : float
    ; alpha : float
    ; max_inventory : int
    ; hedge : bool
    }
  [@@deriving sexp_of]
end

let quotes (p : Params.t) ~fair ~step_sd ~inventory =
  let variance = fair *. (1. -. fair) in
  let reservation = fair -. (Float.of_int inventory *. p.gamma *. variance) in
  let flow_term =
    if Float.(p.gamma < 1e-12) then 1. /. p.k else Float.log1p (p.gamma /. p.k) /. p.gamma
  in
  let half = (p.gamma *. variance /. 2.) +. flow_term +. (p.alpha *. step_sd) in
  (* Outward to whole cents: a bid can only be lowered, an ask raised. The
     small allowances keep a value like 0.3 * 100 = 29.999... on its cent. *)
  let bid = Float.round_down (((reservation -. half) *. 100.) +. 1e-9) /. 100.
  and ask = Float.round_up (((reservation +. half) *. 100.) -. 1e-9) /. 100. in
  ( Option.some_if (Float.(bid >= 0.01) && inventory < p.max_inventory) (Float.min bid 0.98)
  , Option.some_if (Float.(ask <= 0.99) && inventory > -p.max_inventory) (Float.max ask 0.02) )
;;

module Flow = struct
  type t =
    { noise_rate : float
    ; informed_rate : float
    ; lookahead : int
    }
  [@@deriving sexp_of]
end

type outcome =
  { pnl : float
  ; edge : float
  ; noise_fills : int
  ; informed_fills : int
  ; terminal_inventory : int
  ; max_inventory_held : int
  }
[@@deriving sexp_of]

let simulate tree contract (params : Params.t) (flow : Flow.t) ~paths ~seed =
  let open Result.Let_syntax in
  let n = Lattice.steps tree in
  let horizon = Float.of_int n *. Lattice.dt tree in
  let%bind bond = Lattice.price tree Contract.(at horizon one) in
  if Float.(abs (bond -. 1.) > 1e-12)
  then raise_s [%message "Market_maker.simulate assumes zero rates" (bond : float)];
  let%map v = Lattice.values tree contract
  and fwd = Lattice.values tree Contract.(at horizon (scale Obs.Spot one)) in
  let up m j = Lattice.up_probability tree ~step:m ~node:j in
  let rng = Random.State.make [| seed |] in
  let chance p = Float.(Random.State.float rng 1. < p) in
  Array.init paths ~f:(fun _ ->
    (* The path first, so an informed trader can look ahead along it. *)
    let path = Array.create ~len:(n + 1) 0 in
    for m = 0 to n - 1 do
      path.(m + 1) <- (if chance (up m path.(m)) then path.(m) + 1 else path.(m))
    done;
    let q = ref 0
    and cash = ref 0.
    and hedge_gains = ref 0.
    and edge = ref 0.
    and noise_fills = ref 0
    and informed_fills = ref 0
    and max_held = ref 0 in
    let trade ~price ~fair ~buy =
      (* [buy]: the market maker buys one contract at [price]. *)
      if buy
      then (
        incr q;
        cash := !cash -. price;
        edge := !edge +. (fair -. price))
      else (
        decr q;
        cash := !cash +. price;
        edge := !edge +. (price -. fair));
      max_held := Int.max !max_held (Int.abs !q)
    in
    for m = 0 to n - 1 do
      let j = path.(m) in
      let fair = v.(m).(j) in
      let v_up = v.(m + 1).(j + 1)
      and v_down = v.(m + 1).(j)
      and u = up m j in
      let step_sd = Float.sqrt (u *. (1. -. u)) *. Float.abs (v_up -. v_down) in
      (* The hedge for the inventory held as the step starts: short delta
         forwards per contract held. *)
      let hedge =
        if params.hedge
        then (
          let df = fwd.(m + 1).(j + 1) -. fwd.(m + 1).(j) in
          if Float.(df > 0.) then -.Float.of_int !q *. (v_up -. v_down) /. df else 0.)
        else 0.
      in
      let bid, ask = quotes params ~fair ~step_sd ~inventory:!q in
      if chance flow.informed_rate
      then (
        let future = v.(Int.min n (m + flow.lookahead)).(path.(Int.min n (m + flow.lookahead))) in
        match bid, ask with
        | _, Some a when Float.(future > a) ->
          trade ~price:a ~fair ~buy:false;
          incr informed_fills
        | Some b, _ when Float.(future < b) ->
          trade ~price:b ~fair ~buy:true;
          incr informed_fills
        | _ -> ());
      let bid, ask = quotes params ~fair ~step_sd ~inventory:!q in
      Option.iter ask ~f:(fun a ->
        if chance (flow.noise_rate *. Float.exp (-.params.k *. (a -. fair)))
        then (
          trade ~price:a ~fair ~buy:false;
          incr noise_fills));
      Option.iter bid ~f:(fun b ->
        if chance (flow.noise_rate *. Float.exp (-.params.k *. (fair -. b)))
        then (
          trade ~price:b ~fair ~buy:true;
          incr noise_fills));
      hedge_gains := !hedge_gains +. (hedge *. (fwd.(m + 1).(path.(m + 1)) -. fwd.(m).(j)))
    done;
    let payout = v.(n).(path.(n)) in
    { pnl = !cash +. (Float.of_int !q *. payout) +. !hedge_gains
    ; edge = !edge
    ; noise_fills = !noise_fills
    ; informed_fills = !informed_fills
    ; terminal_inventory = !q
    ; max_inventory_held = !max_held
    })
;;
