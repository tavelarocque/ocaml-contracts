(** Price a contract against a single-horizon density.

    A terminal density says how the price is distributed *at one date*. It
    says nothing about the path in between, or about other dates. So some
    contracts are honestly unpriceable under this model, and the pricer says
    so with a typed reason instead of returning a made-up number. *)

open! Core

(** Re-exported, so [Pricer.Unpriceable.t] still works. *)
module Unpriceable = Unpriceable

(** Value today, or why it can't be computed. *)
val price : Density.t -> Contract.t -> (float, Unpriceable.t) Result.t

(** Like [price], but raises on an unpriceable contract. Handy in tests. *)
val price_exn : Density.t -> Contract.t -> float
