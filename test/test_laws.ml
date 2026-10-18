(* Property tests: laws every correct pricer must obey, checked on randomly
   generated contracts.

   An expect test checks one example you thought of. A property test states a
   law ("for ALL contracts c, give (give c) is worth what c is worth") and
   lets Quickcheck hunt for a counterexample among hundreds of random
   contracts, including ugly nested ones nobody would write by hand. If it
   finds one, the test fails and prints the contract that broke the law.

   The generator builds contracts the way the language does: from leaves
   (zero, one, receive $X) combined by give, and, or, scale and cond, then
   wrapped in [at expiry] so the single-date pricer can value them. *)

open! Core
open Contracts
module G = Quickcheck.Generator

let expiry = 7. /. 365.

(* A coarse grid keeps hundreds of trials fast; the laws don't depend on
   grid size. *)
let model =
  Density.lognormal ~n:401 ~spot0:100_000. ~horizon:expiry ~rate:0.04 ~vol:0.55 ()
;;

let price = Pricer.price_exn model

(* Prices can be ~1e5 (forwards) or ~1e-3 (deep digitals), so compare with a
   tolerance relative to size. Summing in a different order legitimately
   changes the last few bits of a float. *)
let close a b = Float.(abs (a - b) <= 1e-9 * (1. + abs a + abs b))

(* --- Generators ---------------------------------------------------------- *)

let strike = Float.gen_incl 80_000. 120_000.

let obs : Contract.Obs.t G.t =
  G.union
    [ G.return Contract.Obs.Spot
    ; G.map (Float.gen_incl (-2.) 2.) ~f:(fun k -> Contract.Obs.Const k)
    ; G.map strike ~f:(fun k -> Contract.Obs.Sub (Spot, Const k))
    ; G.map strike ~f:(fun k -> Contract.Obs.Sub (Const k, Spot))
    ]
;;

let pred : Contract.Pred.t G.t =
  G.union
    [ G.map strike ~f:(fun k -> Contract.Pred.Ge (Spot, Const k))
    ; G.map strike ~f:(fun k -> Contract.Pred.Lt (Spot, Const k))
    ]
;;

let scalar = Float.gen_incl (-3.) 3.

(* What gets acquired at expiry. [recursive_union] grows trees up to the
   current size, so small sizes give small contracts. *)
let payoff : Contract.t G.t =
  G.recursive_union
    [ G.return Contract.zero
    ; G.return Contract.one
    ; G.map obs ~f:(fun o -> Contract.scale o Contract.one)
    ]
    ~f:(fun self ->
      let open G.Let_syntax in
      [ G.map self ~f:Contract.give
      ; G.map2 self self ~f:Contract.and_
      ; G.map2 self self ~f:Contract.or_
      ; G.map2 scalar self ~f:Contract.scale_k
      ; (let%map p = pred
         and a = self
         and b = self in
         Contract.cond p a b)
      ])
;;

(* A contract held today: payoffs at expiry, combined by today's operations. *)
let contract : Contract.t G.t =
  G.recursive_union
    [ G.map payoff ~f:(Contract.at expiry) ]
    ~f:(fun self ->
      [ G.map self ~f:Contract.give
      ; G.map2 self self ~f:Contract.and_
      ; G.map2 self self ~f:Contract.or_
      ; G.map2 scalar self ~f:Contract.scale_k
      ])
;;

(* --- Running a law ------------------------------------------------------- *)

(* Sizes 0-6 give trees of about 8 nodes typically and up to ~90, and
   500 trials per law. *)
let sizes = Sequence.cycle_list_exn [ 0; 1; 2; 3; 4; 5; 6 ]

let check name gen ~sexp_of ~f =
  Quickcheck.test ~trials:500 ~sizes ~sexp_of gen ~f:(fun x ->
    if not (f x) then raise_s [%message "law broken" name]);
  printf "holds: %s\n" name
;;

let one_contract = contract
let sexp_of_one = Contract.sexp_of_t
let two_contracts = G.both contract contract
let sexp_of_two = [%sexp_of: Contract.t * Contract.t]
let two_payoffs = G.both payoff payoff

(* These six laws restate the pricer's own definitions (Give negates, And
   adds, Or-today takes the max...), so today they cannot fail. They are here
   as a guard for later: if the pricer is ever rewritten, say into a faster
   mutable version for Milestone 2, these pin down what it must still do.
   The laws in the next test are the ones with real teeth: they hold only
   because of how [At] averages over the density. *)
let%expect_test "algebraic laws of the contract language" =
  check "give (give c) = c" one_contract ~sexp_of:sexp_of_one ~f:(fun c ->
    close (price (Contract.give (Contract.give c))) (price c));
  check "give c = -c" one_contract ~sexp_of:sexp_of_one ~f:(fun c ->
    close (price (Contract.give c)) (-.price c));
  check "and a b = a + b" two_contracts ~sexp_of:sexp_of_two ~f:(fun (a, b) ->
    close (price (Contract.and_ a b)) (price a +. price b));
  check "and zero c = c" one_contract ~sexp_of:sexp_of_one ~f:(fun c ->
    close (price (Contract.and_ Contract.zero c)) (price c));
  check
    "scale k c = k * c"
    (G.both scalar contract)
    ~sexp_of:[%sexp_of: float * Contract.t]
    ~f:(fun (k, c) -> close (price (Contract.scale_k k c)) (k *. price c));
  check "or a b (chosen today) = max a b" two_contracts ~sexp_of:sexp_of_two
    ~f:(fun (a, b) ->
      close (price (Contract.or_ a b)) (Float.max (price a) (price b)));
  [%expect {|
    holds: give (give c) = c
    holds: give c = -c
    holds: and a b = a + b
    holds: and zero c = c
    holds: scale k c = k * c
    holds: or a b (chosen today) = max a b
    |}]
;;

let%expect_test "laws that involve time" =
  check
    "at T (and a b) = at T a + at T b"
    two_payoffs
    ~sexp_of:sexp_of_two
    ~f:(fun (a, b) ->
      close
        (price (Contract.at expiry (Contract.and_ a b)))
        (price (Contract.at expiry a) +. price (Contract.at expiry b)));
  (* The law behind the straddle test: choosing later, with more information,
     is never worth less than choosing now. *)
  check
    "at T (or a b) >= or (at T a) (at T b)"
    two_payoffs
    ~sexp_of:sexp_of_two
    ~f:(fun (a, b) ->
      let later = price (Contract.at expiry (Contract.or_ a b))
      and now = price (Contract.or_ (Contract.at expiry a) (Contract.at expiry b)) in
      Float.(later >= now - (1e-9 * (1. + abs now))));
  check
    "cond p c c = c"
    (G.both pred payoff)
    ~sexp_of:[%sexp_of: Contract.Pred.t * Contract.t]
    ~f:(fun (p, c) ->
      close (price (Contract.at expiry (Contract.cond p c c))) (price (Contract.at expiry c)));
  check
    "above K is non-increasing in K"
    (G.both strike strike)
    ~sexp_of:[%sexp_of: float * float]
    ~f:(fun (k1, k2) ->
      let lo = Float.min k1 k2
      and hi = Float.max k1 k2 in
      Float.(
        price (Kalshi.above ~expiry ~strike:lo) >= price (Kalshi.above ~expiry ~strike:hi)));
  [%expect {|
    holds: at T (and a b) = at T a + at T b
    holds: at T (or a b) >= or (at T a) (at T b)
    holds: cond p c c = c
    holds: above K is non-increasing in K
    |}]
;;

let%expect_test "a contract with an open Anytime window is never given a price" =
  (* Bury an Anytime with a deadline before the horizon anywhere in a random
     contract: the pricer must refuse the whole thing. *)
  check
    "open Anytime anywhere -> Path_dependent"
    (G.both contract (Float.gen_incl 1e-6 (expiry *. 0.999)))
    ~sexp_of:[%sexp_of: Contract.t * float]
    ~f:(fun (c, deadline) ->
      match Pricer.price model (Contract.and_ c (Contract.anytime deadline Contract.one)) with
      | Error (Path_dependent _) -> true
      | Ok _ | Error _ -> false);
  [%expect {| holds: open Anytime anywhere -> Path_dependent |}]
;;

let%expect_test "a broken law is caught, with a counterexample" =
  (* Sanity check on the checker itself: a false law must fail. "or a b = a"
     is false whenever b is worth more than a. Only our own "law broken"
     error counts as a pass, so an unrelated crash can't fake one. *)
  (match
     Result.try_with (fun () ->
       check "or a b = a (FALSE)" two_contracts ~sexp_of:sexp_of_two ~f:(fun (a, b) ->
         close (price (Contract.or_ a b)) (price a)))
   with
   | Ok () -> print_endline "the checker missed a false law"
   | Error exn ->
     let report = Sexp.to_string [%sexp (exn : exn)] in
     printf "false law rejected: %b; counterexample included: %b\n"
       (String.is_substring report ~substring:"law broken")
       (String.is_substring report ~substring:"input"));
  [%expect {| false law rejected: true; counterexample included: true |}]
;;
