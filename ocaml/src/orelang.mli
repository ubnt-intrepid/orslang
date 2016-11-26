(* orelang.mli *)

type expr =
  | Nil
  | Number of int
  | Symbol of string
  | Command of string * expr list
[@@deriving show]

val parse_from_str: string -> expr option
