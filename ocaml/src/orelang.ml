(* orelang.ml *)

open Angstrom

type expr =
  | Token of string
  | List of expr list
[@@deriving show]

let spaces = skip_while (function
    | ' ' | '\n' | '\t' -> true
    | _ -> false)

let token =
  spaces *>
  take_while1 (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9'
      | '+' | '-' | '*' | '/' | '=' -> true
      | _ -> false)
  <* spaces

let tlist (expr: expr Angstrom.t) =
  spaces *> string "(" *> sep_by spaces expr <* string ")" <* spaces

let expr = fix (fun expr ->
    let token = token      >>| fun t -> Token t in
    let tlist = tlist expr >>| fun l -> List l  in
    token <|> tlist)

let parse_from_str s : expr option =
  match parse_only expr (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
