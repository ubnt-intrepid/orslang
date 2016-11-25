(* orelang.mli *)

type expr =
    Token of string
  | List of expr list
[@@deriving show]

val parse_from_str: string -> expr option
