// Program.fs
open System

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

  let brackets<'a, 'b> : Parser<'a, 'b> -> Parser<'a, 'b> =
      between <| (pstring "(" >>. spaces)  <| (spaces .>> pstring ")")

  let boolean = (pstring "true" >>% Boolean true) <|> (pstring "false" >>% Boolean false)

  let number = regex @"\d+" >>= (fun n -> try Int32.Parse n |> (Number >> preturn)
                                          with | _ -> fail "failed parse as a number")

  let symbol = regex @"[a-zA-Z\+\-\*\/!_][a-zA-Z0-9\+\-\*\/!_]*" |>> Symbol

  let list expr = brackets <| sepEndBy expr spaces1 |>> List

  let expression = fix (fun expr -> boolean <|> number <|> symbol <|> list expr)

  let program = spaces >>. expression .>> spaces .>> eof

  let FromString (s:string) =
      match run program s with
      | ParserResult.Success (res, _, _) -> result.Success res
      | ParserResult.Failure (msg, _, _) -> result.Failure msg


[<EntryPoint>]
let main _ =
   printfn "%A" <| Parser.FromString "hoge"
   printfn "%A" <| Parser.FromString "10"
   printfn "%A" <| Parser.FromString "false"
   printfn "%A" <| Parser.FromString "(a b c + - * / _ !)"
   printfn "%A" <| Parser.FromString "(abc/def a10 (2 3 hoge))"
   printfn "%A" <| Parser.FromString "( 10 fa1   \n 10  )"

   printfn "%A" <| Parser.FromString "(10a)"
   printfn "%A" <| Parser.FromString "a b c"
   0
