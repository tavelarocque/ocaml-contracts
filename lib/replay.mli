(** The market maker's quoting rule replayed against a real book and real
    trade prints.

    At each trade print in a market, the market maker's quotes are worked
    out from the fair value then ([Markout.Fair]) and its inventory in that
    market. A quote that would cross the book at that moment is not posted.
    A taker who bought YES at a price at or above our ask is assumed to have
    filled one contract of our ask; a taker who sold at or below our bid,
    one of our bid.

    That fill rule is optimistic: it assumes our quote was first in the
    queue at its price. Real fills would be fewer, and more often the
    adversely selected ones (a quote left at the front when the price moves
    through it). So read the replay's P&L as an upper bound, and its
    markouts as the more honest measure.

    Each fill is returned as a [Markout.t], with the market maker as the
    passive side, so [Markout.Summary] applies unchanged. Its markout at
    the close is that fill's profit at settlement, so their sum is the
    replay's P&L. *)

open! Core

val run
  :  Markout.Fair.t
  -> Market_maker.Params.t
  -> books:Hf.Book.t list
  -> trades:Hf.Trade.t list
  -> horizons:float list
  -> Markout.t list
