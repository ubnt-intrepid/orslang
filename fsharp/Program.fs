// Program.fs
open System
open System.Collections.Generic

let rec fix f x = f (fix f) x

type result<'t, 'e> = Success of 't
                    | Failure of 'e

module Result =
    let map f x =
        match x with
        | Success s -> Success (f s)
        | Failure e -> Failure e

    let bind f x =
        match x with
        | Success s -> f s
        | Failure e -> Failure e


type expression = Nil
                | Boolean of bool
                | Number of int
                | Symbol of string
                | List of expression list


module Parser =
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers

    let brackets = between <| pstring "("  <| pstring ")"

    let boolean = (pstring "true" >>% Boolean true) <|>
                  (pstring "false" >>% Boolean false)

    let number = regex @"[\+\-]?\d+" |>> (Int32.Parse >> Number)

    let symbol = regex @"[a-zA-Z\+\-\*\/!_][a-zA-Z0-9\+\-\*\/!_]*" |>> Symbol

    let list expr =
        brackets <| (spaces >>. sepEndBy expr spaces1 .>> spaces |>> List)

    let expression = fix <| fun expr ->
        boolean <|> number <|> symbol <|> list expr

    let program = spaces >>. expression .>> spaces .>> eof

    let FromString (s:string) =
        match run program s with
        | ParserResult.Success (res, _, _) -> result.Success res
        | ParserResult.Failure (msg, _, _) -> result.Failure msg


type operator = delegate of expression list -> result<expression, string>

type engine() =
    let variables = Dictionary<string, expression>()
    let operators = Dictionary<string, operator>()

    member this.Variables = variables
    member this.Operators = operators

    member this.Evaluate =
        function
        | Nil -> Success(Nil)
        | Boolean(b) -> Success(Boolean(b))
        | Number(n) -> Success(Number(n))
        | Symbol(s) ->
            match variables.TryGetValue(s) with
            | (true, v) -> Success(v)
            | _ -> Failure(sprintf "undefined symbol: `%s`" s)

        | List [] -> Failure("need one or more elements in the list")
        | List(Symbol(op) :: args) ->
            match operators.TryGetValue(op) with
            | (true, op) -> op.Invoke(args)
            | _ -> Failure(sprintf "undefined operator: `%s`" op)
        | List lst -> Failure(sprintf "invalid argument: `%A`" lst)

type orelangEngine() as this =
    inherit engine()
    with
        do this.Operators.Add("+", fun args ->
                                   match args with
                                   | e1::e2::_ ->
                                     match (this.Evaluate(e1), this.Evaluate(e2)) with
                                     | (Success(Number v1), Success(Number v2)) -> Success(Number(v1 + v2))
                                     | _ -> Failure("[+] cannot substitute")
                                   | _ -> Failure("not implemented"))

[<EntryPoint>]
let main _ =
    // printfn "%A" <| Parser.FromString "hoge"
    // printfn "%A" <| Parser.FromString "10"
    // printfn "%A" <| Parser.FromString "-10"
    // printfn "%A" <| Parser.FromString "false"
    // printfn "%A" <| Parser.FromString "(a b c + - * / _ !)"
    // printfn "%A" <| Parser.FromString "(abc/def a10 (2 3 hoge))"
    // printfn "%A" <| Parser.FromString "( 10 fa1   \n 10  )"

    // printfn "%A" <| Parser.FromString "(10a)"
    // printfn "%A" <| Parser.FromString "a b c"

    let eng = orelangEngine()
    Parser.FromString "(+ 1 2)" |> Result.bind eng.Evaluate |> printfn "%A"
    0
