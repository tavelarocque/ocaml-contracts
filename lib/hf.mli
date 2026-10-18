(** High-frequency Kalshi data: top-of-book changes, trade prints and the
    Deribit BTC index, as exported by Implied Odds'
    [tools/export_hf_for_ocaml.py]. Times are seconds since 1970 (UTC). *)

open! Core

(** "2026-10-17T15:04:05.123456+00:00" or "...Z" to seconds since 1970.
    Only UTC offsets of zero are accepted, which is all the exports use. *)
val parse_utc : string -> float Or_error.t

(** Seconds since 1970 to "YYYY-MM-DDTHH:MM:SS.ffffff+00:00". *)
val to_utc_string : float -> string

module Book : sig
  type t =
    { t : float
    ; event : string
    ; ticker : string
    ; strike : float
    ; close : float
    ; bid : float option
    ; ask : float option
    ; bid_size : float option
    ; ask_size : float option
    }
  [@@deriving sexp_of]
end

module Trade : sig
  type t =
    { t : float
    ; ticker : string
    ; price : float (** YES price in dollars *)
    ; count : float
    ; taker_bought_yes : bool
    }
  [@@deriving sexp_of]
end

val load_books : string -> Book.t list Or_error.t
val load_trades : string -> Trade.t list Or_error.t

(** (time, index) pairs, sorted by time. *)
val load_index : string -> (float * float) array Or_error.t
