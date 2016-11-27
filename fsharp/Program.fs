// Program.fs

open System.IO
open System.Collections.Generic

type Result<'t, 'e> = OK of 't | Error of 'e

let rec fix f x = f (fix f) x

module Orelang =
  type Expr = Nil
            | Boolean of bool
            | Number of decimal
            | Symbol of string
            | Function of string * Expr list
    with
      member this.ToSymbol() =
        match this with
        | Symbol s -> Some s
        | _ -> None

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
    let env = new Dictionary<string, Expr>()

    member private this.getValue k =
      match env.TryGetValue(k) with
      | (true, v) -> Some v
      | _         -> None

    member private this.setValue k v =
      env.Remove(k) |> ignore
      env.Add(k, v)

    member this.evalFunc name (args: Expr list) =
      match name with
      | "set" ->
        try Some (List.item 0 args) with | _ -> None
        |> Option.bind (fun s -> s.ToSymbol())
        |> Option.bind (fun k ->
           try Some (List.item 1 args) with | _ -> None
           |> Option.bind (fun v ->
              this.setValue k v; Some Nil))
      | "until" -> None
      | "step" -> None
      | "+" -> None
      | "=" -> None
      | _ -> None

    member this.Evaluate (expr: Expr) : Expr option =
      match expr with
      | Nil | Boolean _ | Number _  -> Some expr
      | Symbol k                    -> this.getValue k
      | Function (name, args)       -> this.evalFunc name args

    member this.print() =
      printfn "env: %A" env

[<EntryPoint>]
let main _ =
  let testString s =
    printfn "\nstring: %s" s
    match Orelang.ParseFromString s with
    | Result.Error msg ->
      printfn "failed to parse: %A\n" msg
    | Result.OK expr ->
      let engine = Orelang.Engine()
      let result = engine.Evaluate expr
      printfn "result: %A" result
      engine.print()

  let testFile filename =
    printfn "\nfile: %s" filename
    let repo_root = Directory.GetParent(__SOURCE_DIRECTORY__).ToString()
    match Orelang.ParseFromFile <| repo_root + "/examples/" + filename with
    | Result.Error msg ->
      printfn "failed to parse: %A\n" msg
    | Result.OK expr ->
      let engine = Orelang.Engine()
      let result = engine.Evaluate expr
      printfn "result: %A" result
      engine.print()

  testString "nil"
  testString "(+ 1 2 (* 3 4))"
  testString "(set i 10)"
  testFile   "example_sum.ore"

  // failure case
  // test_string   "(+ 1 2 (* 3 4)))"
  0
