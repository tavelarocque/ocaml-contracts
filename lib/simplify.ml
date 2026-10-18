open! Core
open Contract

(* Two functions. [simplify] walks the tree bottom-up: it simplifies every
   child first, then hands the rebuilt node to [rule]. [rule] looks only at
   the top of a node whose children are already as small as they get, and
   applies at most one rewrite there.

   Why it terminates: every rewrite returns a contract strictly smaller than
   its input (it drops a constructor, or two Scales become one). A size can't
   shrink forever, so any chain of rewrites ends.

   Why one pass is enough, with one exception: when a rule fires, its result
   is either a child that was already simplified (R1, R3, R4, R8 return a or
   c as they were), or a constant (R2, R6, R9's first case, R10), or a node
   whose child is already simple and whose new top can't match a rule again
   (R9's second case: At (t, c) with c simplified; if c were Zero or an At
   at the same date it would already have collapsed). The exception is R7:
   merging two scales can produce Scale (Const 1., c), which R5 still
   shrinks, or a scale whose child is another constant scale. So R7's result
   goes back through [rule]. That's the case the idempotence check catches. *)

let rec simplify c =
  match c with
  | Zero | One -> c
  | Give a -> rule (Give (simplify a))
  | And (a, b) -> rule (And (simplify a, simplify b))
  | Or (a, b) -> rule (Or (simplify a, simplify b))
  | Scale (o, a) -> rule (Scale (o, simplify a))
  | Cond (p, a, b) -> rule (Cond (p, simplify a, simplify b))
  | At (t, a) -> rule (At (t, simplify a))
  | Anytime (t, a) -> rule (Anytime (t, simplify a))

and rule c =
  match c with
  (* R1: giving away what you were given is holding it. *)
  | Give (Give a) -> a
  (* R2: nothing, given away, is nothing. *)
  | Give Zero -> Zero
  (* R3: holding nothing alongside a contract adds nothing. *)
  | And (Zero, a) | And (a, Zero) -> a
  (* R4: a choice between two identical contracts is that contract. *)
  | Or (a, b) when Contract.equal a b -> a
  (* R5: scaling by exactly one. *)
  | Scale (Obs.Const k, a) when Float.equal k 1. -> a
  (* R6: any multiple of nothing. *)
  | Scale (_, Zero) -> Zero
  (* R7: two constant scales are one. The result may match R5 or R7 again,
     so it goes back through [rule]; it's smaller, so this ends. *)
  | Scale (Obs.Const x, Scale (Obs.Const y, a)) -> rule (Scale (Obs.Const (x *. y), a))
  (* R8: a condition whose branches agree doesn't matter. *)
  | Cond (_, a, b) when Contract.equal a b -> a
  (* R9: nothing at any date is nothing; acquiring at t a contract that is
     itself acquired at t is acquiring it at t. *)
  | At (_, Zero) -> Zero
  | At (t, At (t', a)) when Float.equal t t' -> At (t, a)
  (* R10: the right to acquire nothing, whenever you like, is nothing. *)
  | Anytime (_, Zero) -> Zero
  | c -> c
;;

(* The three questions from simplify.mli.

   Why isn't [or_ c zero -> c] a rule? Because Or is a choice. or_ c zero is
   an option on c: you take c only when it's worth more than nothing, so its
   value is max(c, 0), not c. A call is exactly or_ (S - K) zero at expiry.

   Why isn't [scale (Const 0.) c -> zero] a rule, though its price is always
   0? Because pricing can fail. If c names a date the model doesn't cover,
   scale 0 c is unpriceable too, and the pricer says so. Rewriting it to
   zero would turn that error into a confident 0 and hide a mistake in the
   contract.

   Why may R9 and R10 make an unpriceable contract priceable, but nothing
   may do the opposite? at t zero fails for a t beyond the model's horizon,
   yet it promises nothing whatever t is, so 0 is its true value; the
   rewrite only removes a question the model couldn't answer and didn't need
   to. Going the other way would refuse to price contracts that have a
   price. And a rewrite must never change a price that exists: that's the
   soundness check. *)

let rec size = function
  | Zero | One -> 1
  | Give c | Scale (_, c) | At (_, c) | Anytime (_, c) -> 1 + size c
  | And (a, b) | Or (a, b) | Cond (_, a, b) -> 1 + size a + size b
;;
