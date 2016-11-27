(* orelang.ml *)

open Angstrom

type expr =
  | Nil
  | Bool of bool
  | Number of int
  | Symbol of string
  | Function of { name: string; args: expr list; }
[@@deriving show]

let spaces = skip_while (function
    | ' ' | '\n' | '\t' -> true
    | _ -> false)

let lparen = string "("
let rparen = string ")"

let ws = spaces

let symbol = take_while1 (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9'
    | '+' | '-' | '*' | '/' | '=' -> true
    | _ -> false)

let nil =
  string "nil" *> return Nil

let _bool =
  string "true" *> return (Bool true)
  <|>
  string "false" *> return (Bool false)

let num_or_sym =
  let to_expr s = try Number (int_of_string s) with | _ -> Symbol s in
  symbol >>| to_expr

let command expr =
  lparen *> ws *> lift2 (fun s a -> Function { name = s; args = a; }) (symbol <* ws) (sep_by ws expr) <* rparen

let expr = fix (fun expr ->
    let token =
      nil
      <|> _bool
      <|> num_or_sym
      <|> command expr
    in
    ws *> token <* ws)

let parse_from_str s : expr option =
  match parse_only expr (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
