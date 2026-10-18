(** Quoting a Kalshi YES contract, and a simulated market to test the
    quotes in.

    The quoting rule is Avellaneda & Stoikov (2008), adapted to a contract
    that settles at $0 or $1, as Feil & Nendel (2026) do. A market maker
    with a fair value p, holding q contracts:

    - quotes around a reservation price r = p - q * gamma * p (1 - p)
      rather than p. With inventory it leans its quotes to sell what it
      holds: long YES means lower quotes, so buyers come and sellers don't.
      p (1 - p) is the variance of a $0/$1 payout, the risk of carrying the
      position to settlement. gamma is risk aversion.
    - sets a half-spread of gamma p (1 - p) / 2 + ln(1 + gamma / k) / gamma.
      The second term trades off fill rate against edge per fill. k says
      how fast order flow dries up as quotes move away from fair value:
      arrivals fall like exp(-k * distance). As gamma -> 0 it is 1 / k.
    - adds alpha times the standard deviation of the fair value's next
      move. Avellaneda-Stoikov assumes nobody knows more than the market
      maker. Against traders who see the next move first, the spread has
      to cover how far the price can jump before the quotes can follow.
      That move is largest for a contract struck near the price, near the
      close: the pin risk of [Hedge]. alpha = 0 is the plain rule.

    Quotes are on Kalshi's 1-cent ticks between 1 and 99 cents, rounded
    outward, and a side is dropped at the inventory limit. Prices here are
    in dollars per contract, so 0.01 is a cent. Maker fees are left out. *)

open! Core

module Params : sig
  type t =
    { gamma : float (** risk aversion; 0 = no inventory skew *)
    ; k : float (** order-flow decay per dollar of distance from fair *)
    ; alpha : float (** spread added per sd of the fair value's next move *)
    ; max_inventory : int (** stop adding to a position this size *)
    ; hedge : bool (** delta-hedge inventory in the forward each step *)
    }
  [@@deriving sexp_of]
end

(** Bid and ask in dollars, or [None] for a side not quoted. [step_sd] is
    the standard deviation of the fair value's next move. *)
val quotes
  :  Params.t
  -> fair:float
  -> step_sd:float
  -> inventory:int
  -> float option * float option

module Flow : sig
  type t =
    { noise_rate : float
    (** chance per step that an uninformed trader would trade on a side
        quoted exactly at fair value; it falls like exp(-k * distance) *)
    ; informed_rate : float
    (** chance per step that an informed trader shows up *)
    ; lookahead : int
    (** how many steps ahead the informed trader sees the fair value. It
        buys when that future value is above the ask and sells when it is
        below the bid. *)
    }
  [@@deriving sexp_of]
end

type outcome =
  { pnl : float (** cash + inventory's payout + hedge gains, in dollars *)
  ; edge : float
  (** sum over fills of the price's distance from fair value at the time,
      in the market maker's favour. Under purely uninformed flow the P&L
      averages exactly this; the gap is what informed traders take. *)
  ; noise_fills : int
  ; informed_fills : int
  ; terminal_inventory : int
  ; max_inventory_held : int
  }
[@@deriving sexp_of]

(** Simulate one market maker per path quoting [contract] (a $0/$1 contract
    paying at the tree's horizon, such as [Kalshi.above]) along random paths
    through the tree. Each step: set the hedge (if hedging) for the
    inventory held now; post quotes; informed then uninformed traders may
    trade; then the price moves. Inventory bought during a step is carried
    unhedged through that step's move, since a hedge can't be placed
    before a fill it doesn't know about yet. Rates are taken as zero. *)
val simulate
  :  Lattice.t
  -> Contract.t
  -> Params.t
  -> Flow.t
  -> paths:int
  -> seed:int
  -> (outcome array, Unpriceable.t) Result.t
