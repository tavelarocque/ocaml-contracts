(** Selling a contract and delta-hedging it, simulated on an implied tree.

    A market maker who sells a Kalshi YES contract can hedge it by trading
    BTC: hold delta units of the price, and adjust as the price moves. In a
    binomial tree that hedge is exact if it is adjusted at every step: each
    step has only two outcomes, and two instruments (cash and BTC) can
    match any payoff over two outcomes. That is why a tree price is a fair
    price, and the first test checks it.

    Real hedgers adjust less often than the price moves. This module
    simulates adjusting only every [rebalance_every] steps, along random
    paths through the tree, and reports what the seller is left with: the
    premium received, plus the hedge's gains, minus what the contract
    pays.

    The hedge trades the forward to the tree's horizon, whose value at each
    node the language itself computes ("receive S_T at the horizon"). Rates
    are taken as zero, as in the Deribit export, so idle cash earns nothing
    and every value is already in today's dollars. *)

open! Core

type path =
  { terminal : float (** the price at the horizon *)
  ; pnl : float (** premium + hedge gains - payoff *)
  }
[@@deriving sexp_of]

(** One path per simulated seller. A European contract that pays at the
    tree's horizon is expected; anything the tree can price works. Raises
    if the tree's rate isn't zero. *)
val simulate
  :  Lattice.t
  -> Contract.t
  -> rebalance_every:int
  -> paths:int
  -> seed:int
  -> (path array, Unpriceable.t) Result.t

(** Mean and standard deviation of the P&L over a set of paths. *)
val mean_sd : path array -> float * float
