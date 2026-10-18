(** A third pricer: an implied binomial tree, after Rubinstein (1994),
    "Implied Binomial Trees", Journal of Finance.

    The other two pricers know only the distribution of the price on one
    date, so they refuse anything whose value depends on the path: an
    [Anytime] window, or "does BTC touch $X before 5pm?". A tree adds time.
    It is a recombining binomial lattice whose final step reproduces a given
    terminal distribution, and whose earlier steps say how the price can
    move to get there.

    Construction:
    - The final nodes sit on a standard binomial spacing in log-price,
      scaled to the distribution's own spread, so a roughly normal
      distribution maps onto roughly even up/down odds and variance builds
      up evenly through time.
    - Each final node gets the probability the distribution puts near it,
      and the average price there, which makes expected values exact.
    - Earlier nodes follow Rubinstein's assumption that every path ending
      at the same node is equally likely. Each earlier price is then the
      (growth-adjusted) average of its two children, so the tree has no
      arbitrage.

    Two things are assumptions rather than data: how variance is spread
    through time (evenly, one step = horizon / steps), and the equal-paths
    rule. A barrier is checked only at the tree's steps, which slightly
    understates how often a continuously watched price would touch it. *)

open! Core

type t

(** A tree with [steps] time steps from today to the model's horizon.
    Fails for fewer than 1 step. *)
val create : steps:int -> Replication.Model.t -> t Or_error.t

val steps : t -> int

(** Years per step. *)
val dt : t -> float

(** The price at a node: [step] from 0 to [steps], [node] from 0 (all down
    moves) to [step] (all up moves). *)
val node_price : t -> step:int -> node:int -> float

(** The probability of an up move from a node. *)
val up_probability : t -> step:int -> node:int -> float

(** The probability of being at a node. *)
val node_probability : t -> step:int -> node:int -> float

(** The final step's prices and probabilities, as a density the grid
    pricer can use. European contracts priced on the tree and by [Pricer]
    on this density agree to rounding. *)
val terminal_density : t -> Density.t

(** Value today. Dates must fall on the tree's steps. Errors are those of
    [Unpriceable], plus [Off_grid] for a date between steps. *)
val price : t -> Contract.t -> (float, Unpriceable.t) Result.t

val price_exn : t -> Contract.t -> float

(** The value of acquiring the contract at every node: [.(step).(node)],
    nodes from all down moves (0) to all up moves ([step]). [price] is the
    single value at step 0. *)
val values : t -> Contract.t -> (float array array, Unpriceable.t) Result.t
