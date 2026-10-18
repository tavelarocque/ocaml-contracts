open! Core

type path =
  { terminal : float
  ; pnl : float
  }
[@@deriving sexp_of]

let simulate tree contract ~rebalance_every ~paths ~seed =
  let open Result.Let_syntax in
  if rebalance_every < 1
  then raise_s [%message "Hedge.simulate: rebalance_every must be >= 1" (rebalance_every : int)];
  let n = Lattice.steps tree in
  let horizon = Float.of_int n *. Lattice.dt tree in
  let forward_contract = Contract.(at horizon (scale Obs.Spot one)) in
  let%bind bond = Lattice.price tree Contract.(at horizon one) in
  if Float.(abs (bond -. 1.) > 1e-12)
  then
    raise_s
      [%message "Hedge.simulate assumes zero rates (a bond worth 1)" (bond : float)];
  let%map v = Lattice.values tree contract
  and fwd = Lattice.values tree forward_contract in
  (* The one-step hedge ratio at a node: the two children's values differ
     by delta times the two children's forwards' difference. *)
  let delta m j =
    let dv = v.(m + 1).(j + 1) -. v.(m + 1).(j)
    and df = fwd.(m + 1).(j + 1) -. fwd.(m + 1).(j) in
    if Float.(df > 0.) then dv /. df else 0.
  in
  let rng = Random.State.make [| seed |] in
  Array.init paths ~f:(fun _ ->
    let j = ref 0
    and holding = ref 0.
    and gains = ref 0. in
    for m = 0 to n - 1 do
      if m % rebalance_every = 0 then holding := delta m !j;
      let u = Lattice.up_probability tree ~step:m ~node:!j in
      let next = if Float.(Random.State.float rng 1. < u) then !j + 1 else !j in
      gains := !gains +. (!holding *. (fwd.(m + 1).(next) -. fwd.(m).(!j)));
      j := next
    done;
    { terminal = Lattice.node_price tree ~step:n ~node:!j
    ; pnl = v.(0).(0) +. !gains -. v.(n).(!j)
    })
;;

let mean_sd ps =
  let n = Float.of_int (Array.length ps) in
  let mean = Array.sum (module Float) ps ~f:(fun p -> p.pnl) /. n in
  let var = Array.sum (module Float) ps ~f:(fun p -> Float.square (p.pnl -. mean)) /. (n -. 1.) in
  mean, Float.sqrt var
;;
