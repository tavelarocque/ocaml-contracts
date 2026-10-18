(** Loading the Implied Odds export ([data/export/ocaml/] in that repo).

    [surfaces.csv] holds one smile per (snapshot, Kalshi event);
    [contracts.csv] one row per Kalshi "BTC above strike" contract, with its
    quotes and the reference probability [p_opt]. Columns are found by their
    header names, so a reordered export still loads correctly. Every bad row
    is reported with its line number. *)

open! Core

module Kind : sig
  type t =
    | Hourly
    | Daily
  [@@deriving sexp, equal]
end

module Surface : sig
  type t =
    { snap_utc : string
    ; event : string
    ; close_utc : string
    ; kind : Kind.t
    ; hours_to_close : float
    ; smile : Smile.t
    }
  [@@deriving sexp_of]
end

module Row : sig
  type t =
    { snap_utc : string
    ; event : string
    ; ticker : string
    ; strike : float
    ; bid : float option
    ; ask : float option (** [None] when Kalshi shows no usable ask *)
    ; bid_size : float option
    ; ask_size : float option
    ; in_range : bool
    ; p_opt : float (** Implied Odds' P(BTC > strike), the reference value *)
    }
  [@@deriving sexp_of]
end

val load_surfaces : string -> Surface.t list Or_error.t
val load_rows : string -> Row.t list Or_error.t

(** Pair each contract row with the smile for its (snapshot, event). Rows
    with no matching smile, and two smiles for the same (snapshot, event),
    are errors. *)
val join : Surface.t list -> Row.t list -> (Surface.t * Row.t) list Or_error.t
