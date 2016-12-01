(* orelang.mli *)

type expression =
  | Nil
  | Boolean of bool
  | Number of int
  | Symbol of string
  | List of expression list
[@@deriving show]

val parse_from_str: string -> expression option
