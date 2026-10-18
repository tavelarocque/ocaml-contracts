(* Check that a high-frequency export loads: counts and time ranges only.

     dune exec bin/check_hf.exe -- ~/CODE/implied-odds/data/export/ocaml_hf

   Prints no prices and computes no fair values, so it is safe to run on
   data inside a sealed window: it compares nothing with the options. *)

open! Core
open Contracts

let range ts =
  match List.min_elt ts ~compare:Float.compare, List.max_elt ts ~compare:Float.compare with
  | Some a, Some b -> sprintf "%s to %s" (Hf.to_utc_string a) (Hf.to_utc_string b)
  | _ -> "empty"
;;

let () =
  let dir =
    match Sys.get_argv () with
    | [| _; dir |] -> dir
    | _ ->
      eprintf "usage: check_hf.exe EXPORT_DIR\n";
      exit 2
  in
  let report name = function
    | Ok (n, ts) -> printf "%-13s %7d rows, %s\n" name n (range ts)
    | Error e ->
      printf "%-13s FAILED: %s\n" name (Error.to_string_hum e);
      exit 1
  in
  report "books.csv"
    (Or_error.map (Hf.load_books (dir ^/ "books.csv")) ~f:(fun l ->
       List.length l, List.map l ~f:(fun b -> b.Hf.Book.t)));
  report "trades.csv"
    (Or_error.map (Hf.load_trades (dir ^/ "trades.csv")) ~f:(fun l ->
       List.length l, List.map l ~f:(fun t -> t.Hf.Trade.t)));
  report "index.csv"
    (Or_error.map (Hf.load_index (dir ^/ "index.csv")) ~f:(fun a ->
       Array.length a, Array.to_list a |> List.map ~f:fst));
  report "surfaces.csv"
    (Or_error.bind (Export.load_surfaces (dir ^/ "surfaces.csv")) ~f:(fun l ->
       Or_error.map
         (List.map l ~f:(fun s -> Hf.parse_utc s.Export.Surface.snap_utc) |> Or_error.combine_errors)
         ~f:(fun ts -> List.length l, ts)))
;;
