(* `dune exec bin/main.exe` prices a toy BTC ladder end to end. *)

open! Core
open Contracts

let () =
  let expiry = 1. /. 365. in
  let model = Density.lognormal ~spot0:100_000. ~horizon:expiry ~rate:0.04 ~vol:0.55 () in
  let edges = List.init 7 ~f:(fun i -> 97_000. +. (1_000. *. Float.of_int i)) in
  printf "Toy BTC ladder, 1 day out, spot $100,000, vol 55%%\n\n";
  printf "Each bucket is a contract like:\n  %s\n\n"
    (Contract.to_english (Kalshi.above ~expiry ~strike:103_000.));
  List.iter (Kalshi.ladder ~expiry ~edges) ~f:(fun (label, contract) ->
    match Pricer.price model contract with
    | Ok v -> printf "  %-20s %5.1f c\n" label (100. *. v)
    | Error e -> print_s [%message label (e : Pricer.Unpriceable.t)])
;;
