(** Dates before which some comparisons of Kalshi data with option fair
    values must not be run, because a pre-registered test in Implied Odds
    will look at them first. A tool that would compare Kalshi quotes or
    trades with the options calls [check] with the times of its data. *)

open! Core

type window =
  { name : string
  ; first : float (** first second covered, UTC *)
  ; last : float (** last second covered *)
  ; opens : float (** the first moment the comparison may run *)
  }

(** Layer 5 (Kalshi BTC contracts settling 2026-10-10 to 2026-11-27; with
    the stock layer's 2026-10-05 start this covers 2026-10-05 to
    2026-11-27), open from 2026-11-30. And the test window in the draft of
    layer 16, 2026-11-28 to 2026-12-27, open from 2026-12-28; if the
    registered version changes those dates, change them here. *)
val windows : window list

(** [Error] naming the first window that still covers any of [times] and
    hasn't opened by [now]. *)
val check : now:float -> float list -> (unit, string) Result.t
