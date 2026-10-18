open! Core

type t =
  | Smile
  | Lognormal_atm
  | Scaled_smile of float
[@@deriving sexp, equal]

let name = function
  | Smile -> "smile"
  | Lognormal_atm -> "lognormal"
  | Scaled_smile f -> sprintf "smile x%.2f" f
;;

let model t smile =
  match t with
  | Smile -> Replication.Model.of_smile smile
  | Scaled_smile factor -> Replication.Model.of_smile (Smile.scale_vol smile ~factor)
  | Lognormal_atm ->
    Replication.Model.lognormal
      ~spot0:(Smile.spot smile)
      ~horizon:(Smile.years smile)
      ~discount:(Smile.discount smile)
      ~forward:(Smile.forward smile)
      ~sigma:(Smile.atm_vol smile)
;;

let defaults = [ Smile; Lognormal_atm; Scaled_smile 0.8 ]
