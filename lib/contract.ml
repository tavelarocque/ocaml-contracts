(* The contract language.

   This is a "deep embedding": a contract is a plain data value (a tree), not
   a function. Because it's data, we can write as many interpreters over it as
   we like — a pricer (pricer.ml), a printer (free, via [@@deriving sexp]), a
   function that lists the dates a contract depends on ([dates] below) — and
   the compiler checks that each interpreter handles every case.

   Design follows Peyton Jones, Eber & Seward, "Composing contracts" (2000),
   trimmed to what a single-horizon density model can price. *)

open! Core

(* Time is measured in years from today. A 7-day contract has time 7/365. *)
type time = float [@@deriving sexp, equal]

(* Observables: quantities whose value is known at some moment. [Spot] is the
   underlying price (e.g. the BTC index) *at the moment of evaluation*. That
   one detail is what makes [At] meaningful: inside [At (t, c)], Spot means
   the price at time t. *)
module Obs = struct
  type t =
    | Spot
    | Const of float
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
  [@@deriving sexp, equal]

  let rec eval t ~spot =
    match t with
    | Spot -> spot
    | Const k -> k
    | Add (a, b) -> eval a ~spot +. eval b ~spot
    | Sub (a, b) -> eval a ~spot -. eval b ~spot
    | Mul (a, b) -> eval a ~spot *. eval b ~spot
  ;;
end

(* Predicates over observables. These are data too (not [float -> bool]
   functions) so that contracts stay printable and comparable. *)
module Pred = struct
  type t =
    | Ge of Obs.t * Obs.t (* a >= b *)
    | Lt of Obs.t * Obs.t (* a <  b *)
    | Both of t * t (* p && q *)
  [@@deriving sexp, equal]

  let rec eval t ~spot =
    match t with
    | Ge (a, b) -> Float.(Obs.eval a ~spot >= Obs.eval b ~spot)
    | Lt (a, b) -> Float.(Obs.eval a ~spot < Obs.eval b ~spot)
    | Both (p, q) -> eval p ~spot && eval q ~spot
  ;;
end

(* The combinators. Every instrument in the project is built from these. *)
type t =
  | Zero (* no rights, no obligations *)
  | One (* receive $1 immediately *)
  | Give of t (* swap sides: what you would receive, you pay *)
  | And of t * t (* hold both *)
  | Or of t * t (* hold whichever is worth more, chosen *now* *)
  | Scale of Obs.t * t (* multiply every payment by an observable *)
  | Cond of Pred.t * t * t (* if predicate holds now, the first; else the second *)
  | At of time * t (* acquire the inner contract at time t *)
  | Anytime of time * t
  (* you must acquire the inner contract, at a moment of your choosing no
     later than time t. Only the timing is optional, as in the paper; to make
     acquiring optional too, put [or_ c zero] inside — that is an
     American-style option. Its value depends on the whole price path. *)
[@@deriving sexp, equal]

(* Smart constructors: lowercase functions so contracts read like sentences.
   [and_] / [or_] carry underscores because [and] / [or] are OCaml keywords. *)
let zero = Zero
let one = One
let give c = Give c
let and_ a b = And (a, b)
let or_ a b = Or (a, b)
let scale o c = Scale (o, c)
let scale_k k c = Scale (Obs.Const k, c)
let cond p a b = Cond (p, a, b)
let at t c = At (t, c)
let anytime t c = Anytime (t, c)
let all cs = List.fold cs ~init:zero ~f:and_

(* A second interpreter: every date the contract names explicitly, sorted and
   de-duplicated. Same recursion shape as the pricer, different meaning.

   For [Anytime (t, c)] we record the deadline t. Strictly, the contract
   depends on every moment up to t, which no finite list can hold; the
   deadline is the date a reader needs, and the pricer is what refuses to
   pretend the moments in between don't matter. *)
let dates t =
  let rec go acc = function
    | Zero | One -> acc
    | Give c | Scale (_, c) -> go acc c
    | And (a, b) | Or (a, b) | Cond (_, a, b) -> go (go acc a) b
    | At (time, c) | Anytime (time, c) -> go (time :: acc) c
  in
  go [] t |> List.dedup_and_sort ~compare:Float.compare
;;

(* A third interpreter: render a contract as English, so a reader can check
   that the tree says what they meant. Times print in days, since every
   contract in this project is short-dated.

   [paren] wraps a sub-contract in brackets unless it is a single word, so
   nested contracts stay readable without drowning in brackets. *)
let number_to_string x =
  (* Ten significant digits: enough that a Kalshi edge like 101999.99 never
     rounds to 102000, while %g's default of six would. *)
  sprintf "%.10g" x
;;

let to_english t =
  let num = number_to_string in
  let when_ time =
    (* Hourly contracts read better in hours than as "day 0.04". *)
    let days = time *. 365. in
    if Float.(days < 1.)
    then sprintf "hour %s" (num (Float.round_decimal (days *. 24.) ~decimal_digits:2))
    else sprintf "day %s" (num (Float.round_decimal days ~decimal_digits:2))
  in
  let rec obs = function
    | Obs.Spot -> "Spot"
    | Const k -> num k
    | Add (a, b) -> sprintf "(%s + %s)" (obs a) (obs b)
    | Sub (a, b) -> sprintf "(%s - %s)" (obs a) (obs b)
    | Mul (a, b) -> sprintf "%s * %s" (obs a) (obs b)
  in
  let rec pred = function
    | Pred.Ge (a, b) -> sprintf "%s >= %s" (obs a) (obs b)
    | Lt (a, b) -> sprintf "%s < %s" (obs a) (obs b)
    | Both (p, q) -> sprintf "%s and %s" (pred p) (pred q)
  in
  let rec go = function
    | Zero -> "nothing"
    | One -> "receive $1"
    (* Special case: "receive $X" reads better than "X times (receive $1)". *)
    | Scale (o, One) -> sprintf "receive $%s" (obs o)
    | Scale (o, c) -> sprintf "%s times %s" (obs o) (paren c)
    | Give c -> sprintf "the other side of %s" (paren c)
    | And (a, b) -> sprintf "both %s and %s" (paren a) (paren b)
    | Or (a, b) -> sprintf "the better of %s or %s" (paren a) (paren b)
    | Cond (p, a, b) -> sprintf "if %s then %s else %s" (pred p) (paren a) (paren b)
    | At (time, c) -> sprintf "at %s: %s" (when_ time) (go c)
    | Anytime (time, c) ->
      sprintf "at a moment you choose, by %s: %s" (when_ time) (go c)
  and paren c =
    match c with
    | Zero | One -> go c
    | _ -> "[" ^ go c ^ "]"
  in
  go t
;;
