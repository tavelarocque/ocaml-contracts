(* Checks for the simplifier exercise (see lib/simplify.mli).

     dune exec exercises/simplify/check.exe

   Prints each example (PASS or FAIL, with what was expected and what came
   back), then the four properties on 500 random contracts each, and exits
   with status 1 if anything fails. When it all passes, these become
   regular expect tests in test/. *)

open! Core
open Contracts
open Contract
module Laws = Contracts_test.Test_laws

let show c = Sexp.to_string (Contract.sexp_of_t c)
let call = scale (Obs.Sub (Spot, Const 100_000.)) one
let p = Pred.Ge (Spot, Const 100_000.)

(* (name, input, expected) *)
let examples =
  [ "R1 double give", give (give one), one
  ; "R1 triple give", give (give (give one)), give one
  ; "R2 give zero", give zero, zero
  ; "R3 bottom-up", and_ zero (and_ one zero), one
  ; "R4 same choice twice", or_ call call, call
  ; "trap: an option is not its payoff", or_ one zero, or_ one zero
  ; "R5 scale by one", scale_k 1. (give one), give one
  ; "R6 scale zero", scale Spot zero, zero
  ; "R7 scales multiply", scale_k 2. (scale_k 3. one), scale_k 6. one
  ; "R7 then R5", scale_k 2. (scale_k 0.5 (give one)), give one
  ; "R8 same branches", cond p one one, one
  ; "R9 at zero", at 1. zero, zero
  ; "R9 at twice", at 1. (at 1. one), at 1. one
  ; "R9 different dates", at 1. (at 2. one), at 1. (at 2. one)
  ; "R10 anytime zero", anytime 1. (give (give zero)), zero
  ; "trap: zero times a contract", scale_k 0. one, scale_k 0. one
  ; "cascade", give (scale_k 1. (give (and_ zero one))), one
  ]
;;

let () =
  let failures = ref 0 in
  printf "Examples\n";
  List.iter examples ~f:(fun (name, input, expected) ->
    let got = Simplify.simplify input in
    if Contract.equal got expected
    then printf "  PASS  %s\n" name
    else (
      incr failures;
      printf "  FAIL  %s\n        input    %s\n        expected %s\n        got      %s\n"
        name (show input) (show expected) (show got)));
  printf "\nProperties (500 random contracts each)\n";
  let property name ~f =
    match
      Quickcheck.test_or_error ~trials:500 ~sizes:Laws.sizes ~sexp_of:Contract.sexp_of_t
        Laws.one_contract ~f:(fun c ->
          if f c then Ok () else Or_error.error_s [%message "fails on" (c : Contract.t)])
    with
    | Ok () -> printf "  PASS  %s\n" name
    | Error e ->
      incr failures;
      printf "  FAIL  %s\n        %s\n" name (Error.to_string_hum e)
  in
  let price = Pricer.price Laws.model in
  property "sound: same price whenever c has one" ~f:(fun c ->
    match price c with
    | Error _ -> true
    | Ok v ->
      (match price (Simplify.simplify c) with
       | Ok v' -> Laws.close v v'
       | Error _ -> false));
  property "never bigger" ~f:(fun c ->
    Simplify.size (Simplify.simplify c) <= Simplify.size c);
  property "idempotent: simplify (simplify c) = simplify c" ~f:(fun c ->
    let s = Simplify.simplify c in
    Contract.equal (Simplify.simplify s) s);
  let shrunk = ref 0
  and before = ref 0
  and after = ref 0 in
  Quickcheck.iter ~trials:500 ~sizes:Laws.sizes Laws.one_contract ~f:(fun c ->
    let s = Simplify.simplify c in
    before := !before + Simplify.size c;
    after := !after + Simplify.size s;
    if Simplify.size s < Simplify.size c then incr shrunk);
  printf "\n%d of 500 random contracts got smaller; total size %d -> %d\n" !shrunk !before !after;
  if !failures = 0
  then printf "\nAll passing.\n"
  else (
    printf "\n%d failing.\n" !failures;
    exit 1)
;;
