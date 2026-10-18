(** Which distribution to price with, built from one smile.

    The pricers only see a [Replication.Model.t]: a digital-probability
    curve and a partial-mean curve. Anything that supplies those two can be
    swapped in, and the same contracts priced under each. That is how to see
    which prices depend on the smile's shape and which only on its centre
    and width. *)

open! Core

type t =
  | Smile (** the smile as exported *)
  | Lognormal_atm
  (** no smile: lognormal with the smile's forward and at-the-money vol,
      in closed form *)
  | Scaled_smile of float
  (** the smile with every vol multiplied by the factor *)
[@@deriving sexp, equal]

val name : t -> string
val model : t -> Smile.t -> Replication.Model.t

(** Smile, the lognormal, and the smile narrowed to 80% of its vols. *)
val defaults : t list
