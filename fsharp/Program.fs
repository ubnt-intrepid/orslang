// Program.fs

open System.IO
open System.Collections.Generic

type Result<'t, 'e> = OK of 't | Error of 'e

let rec fix f x = f (fix f) x

module Orelang =
  type ErrorKind = ParseError of string

  type expr = Nil
            | Boolean of bool
            | Number of decimal
            | Symbol of string
            | Function of string * expr list

  module Expr =
    let ToBoolean e =
        match e with | Boolean b -> Some b | _ -> None

    let ToNumber e =
        match e with | Number d -> Some d | _ -> None

    let ToSymbol e =
        match e with | Symbol s -> Some s | _ -> None

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


  let ParseFromString (s: string) : Result<expr, ErrorKind> =
    P.Parse s

  let ParseFromFile (path: string) : Result<expr, ErrorKind> =
    ParseFromString <| System.IO.File.ReadAllText path

  type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x

  let maybe = new MaybeBuilder()

  let (>>=) x f = x |> Option.bind f

  let (>>) x y = x |> Option.bind (fun _ -> y)

  type Engine() =
    let env = new Dictionary<string, expr>()

    member private this.getValue k =
      match env.TryGetValue(k) with
      | (true, v) -> Some v
      | _         -> None

    member private this.setValue k v =
      env.Remove(k) |> ignore
      env.Add(k, v)

    member this.evalFunc name args =
      match name with
      | "set" -> maybe {
          let! k = List.tryItem 0 args >>= Expr.ToSymbol
          let! v = List.tryItem 1 args >>= this.Evaluate
          this.setValue k v
          return Nil
        }

      | "until" ->
         let rec f pred expr =
           match this.Evaluate pred with
           | Some(Boolean true) -> Some Nil
           | _ ->
              this.Evaluate expr |> ignore
              f pred expr
         maybe {
           let! pred = List.tryItem 0 args
           let! expr = List.tryItem 1 args
           f pred expr |> ignore
           return Nil
         }

      | "step" ->
        let rec eval_step lines =
          let ret = this.Evaluate(List.head lines)
          match (List.tail lines) with
          | [] -> ret
          | tl -> eval_step tl
        eval_step args

      | "+" -> maybe {
          let! lhs = List.tryItem 0 args >>= this.Evaluate >>= Expr.ToNumber
          let! rhs = List.tryItem 1 args >>= this.Evaluate >>= Expr.ToNumber
          return Number (lhs + rhs)
        }

      | "=" -> maybe {
          let! lhs = List.tryItem 0 args >>= this.Evaluate >>= Expr.ToNumber
          let! rhs = List.tryItem 1 args >>= this.Evaluate >>= Expr.ToNumber
          return Boolean (lhs = rhs)
        }

      | "print" -> maybe {
          let! arg = List.tryItem 0 args >>= this.Evaluate
          printfn "%A" arg
          return Nil
        }

      | _ -> None

    member this.Evaluate (expr: expr) : expr option =
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
