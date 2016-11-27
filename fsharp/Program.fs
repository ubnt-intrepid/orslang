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

  module E =
    let ToBoolean e =
        match e with | Expr.Boolean b -> Some b | _ -> None

    let ToNumber e =
        match e with | Number d -> Some d | _ -> None

    let ToSymbol e =
        match e with | Symbol s -> Some s | _ -> None

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

  type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x

  let maybe = new MaybeBuilder()

  let (>>=) x f = x |> Option.bind f

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
      | "set" -> maybe {
          let! k = List.tryItem 0 args >>= E.ToSymbol
          let! v = List.tryItem 1 args
          this.setValue k v
          return Nil
        }

      | "until" -> maybe {
          let! pred = List.tryItem 0 args
          let! expr = List.tryItem 1 args
          seq {
            while true do
              yield
                match this.Evaluate pred >>= E.ToBoolean with
                | Some false -> true
                | _ -> false
            done
          } |> Seq.takeWhile (fun s -> s)
            |> Seq.fold (fun _ _ -> this.Evaluate expr |> ignore; true) true
            |> ignore
          return Nil
        }

      | "step" -> None

      | "+" -> maybe {
          let! lhs = List.tryItem 0 args >>= E.ToNumber
          let! rhs = List.tryItem 1 args >>= E.ToNumber
          return Number (lhs + rhs)
        }

      | "=" -> maybe {
          let! lhs = List.tryItem 0 args >>= E.ToNumber
          let! rhs = List.tryItem 1 args >>= E.ToNumber
          return Boolean (lhs = rhs)
        }

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
  testString "(until (= 1 1) (set i 10))"
  testFile   "example_sum.ore"

  // failure case
  // test_string   "(+ 1 2 (* 3 4)))"
  0
