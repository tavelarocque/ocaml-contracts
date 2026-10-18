open! Core

type piece =
  { lo : float
  ; hi : float
  ; a : float
  ; b : float
  }
[@@deriving sexp_of]

type t = piece list [@@deriving sexp_of]

module Error = struct
  type t =
    | Unpriceable of Unpriceable.t
    | Nonlinear of Contract.Obs.t
  [@@deriving sexp_of, equal]
end

(* A straight line a + b * S, the building block of every piece. *)
type line =
  { intercept : float
  ; slope : float
  }

let at (l : line) x = l.intercept +. (l.slope *. x)
let piece ~lo ~hi (l : line) = { lo; hi; a = l.intercept; b = l.slope }
let line_of (p : piece) = { intercept = p.a; slope = p.b }
let constant c = [ { lo = 0.; hi = Float.infinity; a = c; b = 0. } ]

let eval (f : t) x =
  match List.find f ~f:(fun p -> Float.(p.lo <= x && x < p.hi)) with
  | Some p -> p.a +. (p.b *. x)
  | None -> raise_s [%message "Payoff.eval: price outside (0, infinity)" (x : float)]
;;

(* Merge neighbouring pieces that carry the same line, so printed payoffs
   stay short. *)
let simplify (f : t) : t =
  List.fold f ~init:[] ~f:(fun (acc : piece list) (p : piece) ->
    match acc with
    | q :: rest when Float.equal q.a p.a && Float.equal q.b p.b -> { q with hi = p.hi } :: rest
    | _ -> p :: acc)
  |> List.rev
;;

(* The core operation. Cut [0, infinity) at every breakpoint of every input,
   so that on each resulting interval each input is a single line, then let
   [f] build the output piece(s) for that interval from those lines.

   Each input's pieces are walked in step with the intervals rather than
   looked up by a test price: the midpoint of two adjacent floats rounds
   onto one of them, so a lookup can miss, while the walk never does. *)
let combine (fs : t list) ~(f : lo:float -> hi:float -> line list -> piece list) : t =
  let cuts =
    List.concat_map fs ~f:(List.map ~f:(fun (p : piece) -> p.hi))
    |> List.filter ~f:Float.is_finite
    |> List.dedup_and_sort ~compare:Float.compare
  in
  let edges = (0. :: cuts) @ [ Float.infinity ] in
  (* Drop the pieces that end at or before [lo]; the next one covers [lo, hi). *)
  let rec from lo (ps : t) =
    match ps with
    | p :: rest when Float.(p.hi <= lo) -> from lo rest
    | ps -> ps
  in
  List.zip_exn (List.drop_last_exn edges) (List.tl_exn edges)
  |> List.folding_map ~init:fs ~f:(fun remaining (lo, hi) ->
    let remaining = List.map remaining ~f:(from lo) in
    remaining, f ~lo ~hi (List.map remaining ~f:(fun ps -> line_of (List.hd_exn ps))))
  |> List.concat
  |> simplify
;;

let combine2 f g ~op =
  combine [ f; g ] ~f:(fun ~lo ~hi -> function
    | [ l1; l2 ] -> op ~lo ~hi l1 l2
    | _ -> assert false (* combine returns one line per input *))
;;

let combine3 f g h ~op =
  combine [ f; g; h ] ~f:(fun ~lo ~hi -> function
    | [ l1; l2; l3 ] -> op ~lo ~hi l1 l2 l3
    | _ -> assert false (* combine returns one line per input *))
;;

let add f g =
  combine2 f g ~op:(fun ~lo ~hi l1 l2 ->
    [ piece ~lo ~hi { intercept = l1.intercept +. l2.intercept; slope = l1.slope +. l2.slope } ])
;;

(* Which of two lines is larger on [lo, hi), given they don't cross inside
   it. On a finite interval, test the middle. On [lo, infinity) the steeper
   line wins eventually, so it wins throughout. *)
let larger_on ~lo ~hi l1 l2 =
  if Float.is_finite hi
  then (
    let x = lo +. ((hi -. lo) /. 2.) in
    if Float.(at l1 x >= at l2 x) then l1 else l2)
  else if Float.(l1.slope > l2.slope)
  then l1
  else if Float.(l2.slope > l1.slope)
  then l2
  else if Float.(l1.intercept >= l2.intercept)
  then l1
  else l2
;;

(* The larger of two lines on [lo, hi). If they cross inside the interval,
   the result switches line at the crossing point, which becomes a new
   breakpoint. This is how a call's kink at the strike appears. *)
let max f g =
  combine2 f g ~op:(fun ~lo ~hi l1 l2 ->
    let cross =
      if Float.equal l1.slope l2.slope
      then None
      else Some ((l2.intercept -. l1.intercept) /. (l1.slope -. l2.slope))
    in
    match cross with
    | Some x when Float.(lo < x && x < hi) ->
      [ piece ~lo ~hi:x (larger_on ~lo ~hi:x l1 l2); piece ~lo:x ~hi (larger_on ~lo:x ~hi l1 l2) ]
    | _ -> [ piece ~lo ~hi (larger_on ~lo ~hi l1 l2) ])
;;

let negate (f : t) = List.map f ~f:(fun p -> { p with a = -.p.a; b = -.p.b })

(* An observable is linear in the price unless two price-dependent
   quantities are multiplied together. *)
let rec linear (o : Contract.Obs.t) : (line, Error.t) Result.t =
  let open Result.Let_syntax in
  match o with
  | Spot -> return { intercept = 0.; slope = 1. }
  | Const k -> return { intercept = k; slope = 0. }
  | Add (x, y) ->
    let%map x = linear x
    and y = linear y in
    { intercept = x.intercept +. y.intercept; slope = x.slope +. y.slope }
  | Sub (x, y) ->
    let%map x = linear x
    and y = linear y in
    { intercept = x.intercept -. y.intercept; slope = x.slope -. y.slope }
  | Mul (x, y) ->
    let%bind lx = linear x
    and ly = linear y in
    if Float.equal lx.slope 0.
    then return { intercept = lx.intercept *. ly.intercept; slope = lx.intercept *. ly.slope }
    else if Float.equal ly.slope 0.
    then return { intercept = ly.intercept *. lx.intercept; slope = ly.intercept *. lx.slope }
    else Error (Error.Nonlinear o)
;;

(* (alpha + beta S)(a + b S) stays linear only if beta or b is zero. *)
let scale (o : Contract.Obs.t) (f : t) =
  let open Result.Let_syntax in
  let%bind { intercept = alpha; slope = beta } = linear o in
  List.map f ~f:(fun p ->
    if Float.(beta <> 0. && p.b <> 0.)
    then Error (Error.Nonlinear o)
    else Ok { p with a = alpha *. p.a; b = (alpha *. p.b) +. (beta *. p.a) })
  |> Result.all
;;

(* A predicate as an indicator: a payoff that is 1 where it holds, 0 where
   not. "x >= y" is "x - y >= 0", and x - y is a line, so it holds on one
   side of a single threshold price c.

   Pieces include their left end, so "S >= c" is exactly [c, infinity).
   "S <= c" must include c, so its cut goes at the next float above c: no
   float lies strictly between, so [0, next c) is exactly "S <= c". Getting
   the endpoint right matters for a density with an atom exactly at c. *)
let rec indicator (p : Contract.Pred.t) : (t, Error.t) Result.t =
  let open Result.Let_syntax in
  let step ~below ~above c =
    if Float.(c <= 0.)
    then constant above
    else [ { lo = 0.; hi = c; a = below; b = 0. }; { lo = c; hi = Float.infinity; a = above; b = 0. } ]
  in
  let ge x y =
    let%map d = linear (Contract.Obs.Sub (x, y)) in
    if Float.equal d.slope 0.
    then constant (if Float.(d.intercept >= 0.) then 1. else 0.)
    else (
      let c = -.d.intercept /. d.slope in
      if Float.(d.slope > 0.)
      then step ~below:0. ~above:1. c
      else step ~below:1. ~above:0. (Float.one_ulp `Up c))
  in
  match p with
  | Ge (x, y) -> ge x y
  | Lt (x, y) ->
    let%map g = ge x y in
    List.map g ~f:(fun (q : piece) -> { q with a = 1. -. q.a })
  | Both (p, q) ->
    let%map p = indicator p
    and q = indicator q in
    combine2 p q ~op:(fun ~lo ~hi l1 l2 ->
      [ piece ~lo ~hi { intercept = l1.intercept *. l2.intercept; slope = 0. } ])
;;

let of_contract ~horizon c =
  let open Result.Let_syntax in
  let rec go (c : Contract.t) =
    match c with
    | Zero -> return (constant 0.)
    | One -> return (constant 1.)
    | Give c -> go c >>| negate
    | And (x, y) ->
      let%map x = go x
      and y = go y in
      add x y
    | Or (x, y) ->
      let%map x = go x
      and y = go y in
      max x y
    | Scale (o, c) -> go c >>= scale o
    | Cond (p, x, y) ->
      (* Only compile a branch that some price can reach: a branch that is
         never taken can't make the contract unpriceable. *)
      let%bind ind = indicator p in
      (match ind with
       | [ only ] when Float.equal only.a 1. -> go x
       | [ only ] when Float.equal only.a 0. -> go y
       | _ ->
         let%map x = go x
         and y = go y in
         combine3 ind x y ~op:(fun ~lo ~hi i lx ly ->
           [ piece ~lo ~hi (if Float.(i.intercept > 0.5) then lx else ly) ]))
    | At (t, c) ->
      (match Unpriceable.at_rule ~now:horizon ~horizon t with
       | `Now -> go c
       | `Error e -> Error (Error.Unpriceable e)
       | `Horizon ->
         (* Unreachable: we are at the horizon, so a date equal to it is
            [`Now], which [at_rule] checks first. *)
         assert false)
    | Anytime (t, c) ->
      (match Unpriceable.anytime_rule ~now:horizon ~horizon t with
       | `Now -> go c
       | `Error e -> Error (Error.Unpriceable e))
  in
  go c
;;
