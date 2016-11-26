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

let _token = ws *> symbol <* ws

let token =
  _token >>| (fun t ->
      match t with
      | "nil" -> Nil
      | t -> try Number (int_of_string t)
        with
        | _ -> Symbol t)

let command expr =
  ws *> lparen *> lift2 (fun t a -> Command (t, a)) _token (sep_by ws expr) <* rparen <* ws

let expr =
  fix (fun expr -> token <|> command expr)

let parse_from_str s : expr option =
  match parse_only expr (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
