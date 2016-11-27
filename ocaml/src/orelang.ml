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

let _symbol = take_while1 (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9'
    | '+' | '-' | '*' | '/' | '=' -> true
    | _ -> false)


let nil =
  string "nil" *> return Nil

let boolean =
  string "true" *> return (Bool true)
  <|>
  string "false" *> return (Bool false)

let number = _symbol
  >>= (fun s ->
      try return (Number (int_of_string s))
      with | _ -> fail "number")

let symbol =
  _symbol >>| fun s -> Symbol s

let _function expr =
  let _function s a = Function { name = s;
                                 args = a; } in
  lparen *> ws *> lift2 _function (_symbol <* ws) (sep_by ws expr) <* rparen

let expr = fix (fun expr ->
    ws *> (
      nil
      <|> boolean
      <|> number
      <|> symbol
      <|> _function expr
    ) <* ws)

let parse_from_str s : expr option =
  match parse_only expr (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
