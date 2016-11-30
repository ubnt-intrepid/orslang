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


type expression = Boolean of bool
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
        brackets <| spaces >>. sepEndBy expr spaces1 .>> spaces |>> List

    let expression = fix <| fun expr ->
        boolean <|> number <|> symbol <|> list expr

    let program = spaces >>. expression .>> spaces .>> eof

    let FromString (s:string) =
        match run program s with
        | ParserResult.Success (res, _, _) -> result.Success res
        | ParserResult.Failure (msg, _, _) -> result.Failure msg


type operator = delegate of expression list -> result<expression, string>

type engine() =
    let ops = Dictionary<string, operator>()
    let vars = Dictionary<string, expression>()

    member this.Evaluate expr = Failure "not implemented"


[<EntryPoint>]
let main _ =
    printfn "%A" <| Parser.FromString "hoge"
    printfn "%A" <| Parser.FromString "10"
    printfn "%A" <| Parser.FromString "-10"
    printfn "%A" <| Parser.FromString "false"
    printfn "%A" <| Parser.FromString "(a b c + - * / _ !)"
    printfn "%A" <| Parser.FromString "(abc/def a10 (2 3 hoge))"
    printfn "%A" <| Parser.FromString "( 10 fa1   \n 10  )"

    printfn "%A" <| Parser.FromString "(10a)"
    printfn "%A" <| Parser.FromString "a b c"

    let eng = engine()
    Parser.FromString "(+ 1 2)" |> eng.Evaluate |> printfn "%A"
    0
