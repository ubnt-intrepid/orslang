(* orelang.ml *)

open Angstrom

type expr =
  | Token of string
  | List of expr list
[@@deriving show]

let spaces = skip_while (function
    | ' ' | '\n' | '\t' -> true
    | _ -> false)

let token = take_while1 (function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '0' .. '9' -> true
    | '+' | '-' | '*' | '/' | '=' -> true
    | _ -> false)
  >>| fun t -> Token t

let expr = fix (fun expr ->
    let tlist = (string "(" *> sep_by spaces expr <* string ")"
                 >>| fun l -> List l)
    in
    token <|> tlist)

let parse_from_str s : expr option =
  match parse_only expr (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
