(* orelang.ml *)

open Angstrom

type expression =
  | Nil
  | Boolean of bool
  | Number of int
  | Symbol of string
  | List of expression list
[@@deriving show]


let (>>%) x y = x *> return y

let between p1 p2 p = p1 *> p <* p

let space = skip (function
    | ' ' | '\n' | '\t' -> true
    | _ -> false)

let ws = skip_many space

let ws1 = skip_many1 space


let brackets = between (string "(") (string ")")

let nil = string "nil" >>% Nil

let boolean = (string "true" >>% Boolean true) <|>
              (string "false" >>% Boolean false)

let sign = string "+" <|> string "-"

let digits = take_while1 (function
    | '0'..'9' -> true
    | _ -> false)

let number =
  lift2 (fun a b -> a ^ b) sign digits <|> digits
  >>| fun s -> Number(int_of_string s)

let symbol = take_while1 (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
    | '+' | '-' | '*' | '/' | '!' | '=' | '_' -> true
    | _ -> false) >>| (fun s -> Symbol s)

let list expr =
  brackets (ws *> sep_by ws1 expr <* ws) >>| (fun l -> List(l))

let expression = fix (fun expr ->
    boolean <|> number <|> symbol <|> list expr)

let parse_from_str s : expression option =
  match parse_only expression (`String s) with
  | Result.Ok v -> Some v
  | Result.Error msg  -> failwith msg
