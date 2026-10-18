open! Core

type t =
  { value : float
  ; delta : float
  ; gamma : float
  ; vega : float
  }
[@@deriving sexp_of]

let compute ?relative_bump ~sticky choice smile contract =
  let open Result.Let_syntax in
  let price s = Replication.price (Model_choice.model choice s) contract in
  let f = Smile.forward smile in
  let h =
    match relative_bump with
    | Some r -> f *. r
    | None ->
      (* A fiftieth of the distribution's standard deviation: small next to
         anything the price of a contract can do, large next to the $10
         step inside each digital's own finite difference. *)
      f *. Smile.atm_vol smile *. Float.sqrt (Smile.years smile) /. 50.
  in
  let moved df = Smile.with_forward smile ~forward:(f +. df) ~sticky in
  let%map value = price smile
  and up = price (moved h)
  and down = price (moved (-.h))
  and vol_up = price (Smile.scale_vol smile ~factor:1.01)
  and vol_down = price (Smile.scale_vol smile ~factor:0.99) in
  { value
  ; delta = (up -. down) /. (2. *. h)
  ; gamma = (up -. (2. *. value) +. down) /. (h *. h)
  ; vega = (vol_up -. vol_down) /. 2.
  }
;;

let compute_exn ?relative_bump ~sticky choice smile contract =
  match compute ?relative_bump ~sticky choice smile contract with
  | Ok g -> g
  | Error e -> raise_s [%message "Greeks.compute" (e : Payoff.Error.t)]
;;
