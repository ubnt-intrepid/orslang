(* orelang.mli *)

type expr =
  | Nil
  | Bool of bool
  | Number of int
  | Symbol of string
  | Function of { name: string; args: expr list; }
[@@deriving show]

val parse_from_str: string -> expr option
