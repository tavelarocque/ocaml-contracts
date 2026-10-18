open! Core

type window =
  { name : string
  ; first : float
  ; last : float
  ; opens : float
  }

let at s = Hf.parse_utc s |> Or_error.ok_exn

let windows =
  [ { name = "Implied Odds layer 5 (Kalshi vs options)"
    ; first = at "2026-10-05T00:00:00Z"
    ; last = at "2026-11-27T23:59:59.999999Z"
    ; opens = at "2026-11-30T00:00:00Z"
    }
  ; { name = "Implied Odds layer 16 test window (draft)"
    ; first = at "2026-11-28T00:00:00Z"
    ; last = at "2026-12-27T23:59:59.999999Z"
    ; opens = at "2026-12-28T00:00:00Z"
    }
  ]
;;

let check ~now times =
  match
    List.find windows ~f:(fun w ->
      Float.(now < w.opens)
      && List.exists times ~f:(fun t -> Float.(t >= w.first && t <= w.last)))
  with
  | None -> Ok ()
  | Some w ->
    Error
      (sprintf "sealed: the data covers %s, which opens on %s" w.name
         (String.prefix (Hf.to_utc_string w.opens) 10))
;;
