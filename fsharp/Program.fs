// Program.fs

type Result<'t, 'e> = OK of 't | Error of 'e

let rec fix f x = f (fix f) x

module Orelang =
  type Expr = Nil
            | Number of decimal
            | Symbol of string
            | Command of string * Expr list

  type ErrorKind = ParseError of string

  module P =
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers
    open System

    type Result = Result

    let private nil = pstring "nil" >>% Nil

    let private number = regex "[+-]?[0-9]+" |>> (Decimal.Parse >> Number)

    let private symbol = regex "[a-zA-Z0-9\+\-\*\/\=\!]+"

    let private token =
      spaces >>. (nil <|> number <|> (symbol |>> Symbol)) .>> spaces

    let private command expr =
      pstring "(" >>. spaces >>. symbol .>> spaces .>>. sepEndBy expr spaces .>> pstring ")"
      |>> fun (symbol, args) -> Command (symbol, args)

    let private expr =
      fix <| fun expr -> token <|> command expr

    let Parse s =
      match run (spaces >>. expr .>> spaces .>> eof) s with
      | Success (r, _, _)   -> OK r
      | Failure (msg, _, _) -> Result.Error <| ParseError msg


  let ParseFromString (s: string) : Result<Expr, ErrorKind> =
    P.Parse s

  let ParseFromFile (path: string) : Result<Expr, ErrorKind> =
    ParseFromString <| System.IO.File.ReadAllText path


[<EntryPoint>]
let main _ =
  let test_string s =
    match Orelang.ParseFromString s with
    | Result.OK ast -> printfn "string:\n%A\n" ast
    | Result.Error msg -> printfn "failed to parse: %A\n" msg

  let test_file path =
    match Orelang.ParseFromFile path with
    | Result.OK ast -> printfn "%s:\n%A\n" <| System.IO.Path.GetFileName path <| ast
    | Result.Error msg -> printfn "failed to parse: %A\n" msg

  test_string   "nil"
  test_string   "(+ 1 2 (* 3 4))"
  test_file     "../examples/example_sum.ore"

  test_string   "(+ 1 2 (* 3 4)))"
  0
