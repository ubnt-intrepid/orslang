// Program.fs

type Result<'t, 'e> = OK of 't | Error of 'e

let rec fix f x = f (fix f) x

module Orelang =
  type Expr = Nil
            | Number of decimal
            | Symbol of string
            | Function of string * Expr list

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
      |>> fun (symbol, args) -> Function (symbol, args)

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

  type Engine() =
    let env = new System.Collections.Generic.Dictionary<string, Expr>()

    member private this.getValue k =
      match env.TryGetValue(k) with
      | (true, v) -> Some v
      | _         -> None

    member private this.setValue k v =
      env.Remove(k) |> ignore
      env.Add(k, v)

    member this.Evaluate (expr: Expr) : Expr option =
      Some expr


open System.IO

[<EntryPoint>]
let main _ =
  let repo_root = Directory.GetParent(__SOURCE_DIRECTORY__).ToString()
  
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
  test_file  <| repo_root + "/examples/example_sum.ore"

  match Orelang.ParseFromFile <| repo_root + "/examples/example_sum.ore" with
  | Result.Error msg ->
    printfn "failed to parse: %A\n" msg
  | Result.OK expr ->
    let engine = Orelang.Engine()
    let result = engine.Evaluate expr
    printfn "result: %A" result

  // failure case
  // test_string   "(+ 1 2 (* 3 4)))"
  0
