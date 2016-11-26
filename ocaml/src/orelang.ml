(* orelang.ml *)

open Angstrom

type expr =
  | Nil
  | Number of int
  | Symbol of string
  | Command of string * expr list
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
  string "nil" >>| fun _ -> Nil

let num_or_sym =
  let to_expr s = try Number (int_of_string s) with | _ -> Symbol s in
  symbol >>| to_expr

let token =
  nil <|> num_or_sym

let command expr =
  lparen *> ws *> lift2 (fun t a -> Command (t, a)) (symbol <* ws) (sep_by ws expr) <* rparen

let expr =
  fix (fun expr -> ws *> (token <|> command expr) <* ws)

let parse_from_str s : expr option =
  match parse_only expr (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
