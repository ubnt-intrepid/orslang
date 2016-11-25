// Program.fs

type ('t, 'e) Result = OK of 't | Error of 'e

let rec fix f x = f (fix f) x

module Parse =
  open FParsec
  open FParsec.Primitives
  open FParsec.CharParsers
  open System.IO

  type Expr = Token of string
            | TList of Expr list

  type ErrorKind = ParseError of string

  let private token =
    regex "[a-zA-Z0-9\+\-\*\/\=\!]+" |>> Token

  let private plist expr =
    between <| pstring "(" <| pstring ")" <| sepEndBy expr spaces |>> TList

  let private expr =
    fix <| fun expr -> token <|> plist expr

  let private parseFromString s =
    run (spaces >>. expr .>> spaces .>> eof) s


  let FromString (s: string) : Result<Expr, ErrorKind> =
    match parseFromString s with
    | Success (r, _, _)   -> OK r
    | Failure (msg, _, _) -> Result.Error <| ParseError msg

  let FromFile (path: string) : Result<Expr, ErrorKind> =
    FromString <| File.ReadAllText path


[<EntryPoint>]
let main _ =
  let test_string s =
    printfn "string: %A" <| Parse.FromString s

  let test_file path =
    printfn "%s: %A" <| System.IO.Path.GetFileName path <| Parse.FromFile path

  test_string   "(+ 1 2 (* 3 4))"
  test_file     "../../examples/example_sum.ore"
  0
