(** A contract simplifier. (Set as an exercise; the implementation in
    simplify.ml answers the questions at the end of this spec.)

    [simplify c] rewrites [c] into a smaller contract that means the same
    thing, using laws of the language. It's the fifth interpreter over the
    contract tree (after the printer, [dates], [to_english] and the
    pricers), and the first that returns a contract rather than a value.

    {1 The rules}

    Apply these, and only these:

    {v
     R1  give (give c)                  ->  c
     R2  give zero                      ->  zero
     R3  and_ zero c,  and_ c zero      ->  c
     R4  or_ a b      when a = b        ->  a
     R5  scale (Const 1.) c             ->  c
     R6  scale o zero                   ->  zero          (any observable o)
     R7  scale (Const a) (scale (Const b) c)
                                        ->  scale (Const (a *. b)) c
     R8  cond p a b   when a = b        ->  a
     R9  at t zero                      ->  zero
         at t (at t' c) when t = t'     ->  at t c
     R10 anytime t zero                 ->  zero
    v}

    "a = b" is [Contract.equal]. Times are floats; compare them with
    [Float.equal] here (the rules only need exact matches).

    {1 What the tests demand}

    [dune exec exercises/simplify/check.exe] checks:

    - 17 examples with known answers, including two traps: contracts that
      look simplifiable but aren't.
    - Soundness, on 500 random contracts: whenever [c] has a price,
      [simplify c] has the same price.
    - It never makes a contract bigger ([size]).
    - It's idempotent: simplifying twice gives the same as once. This is
      the one that catches a simplifier that applies one rule and stops.
      Think about [scale_k 2. (scale_k 0.5 (give one))]: R7 gives
      [scale_k 1. (give one)], which R5 can still shrink.

    {1 Hints}

    - One [match] with a case per rule, on a contract whose children are
      already simplified (recursion first, rules second).
    - When a rule fires, its result may match another rule. Each rule makes
      the contract strictly smaller, so re-simplifying the result
      terminates. Why does that guarantee termination? It's the whole
      argument, and worth one sentence in your comment.
    - Write [when] guards for R4, R8 and R9.

    {1 Questions to answer in a comment once it passes}

    - Why is [or_ c zero -> c] NOT a rule? (Example 6.)
    - Why is [scale (Const 0.) c -> zero] NOT a rule, even though its price
      is always 0? (Example 15. Hint: what if [c] is unpriceable?)
    - R9 and R10 can turn an unpriceable contract into a priceable one.
      Why is that acceptable, when the opposite would not be? *)

open! Core

val simplify : Contract.t -> Contract.t

(** The number of constructors in a contract. Provided. *)
val size : Contract.t -> int
