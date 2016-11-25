// Program.fs

type ('t, 'e) Result = OK of 't | Error of 'e

module Parse =
  open FParsec
  open FParsec.Primitives
  open FParsec.CharParsers
  open System.IO

  type Expr = Token of string
            | List of Expr list

  type ErrorKind = ParseError of string

  type ExprParser = Parser<Expr, unit>

  let private expr, exprRef = createParserForwardedToRef<Expr, unit>()

  let private token : ExprParser = regex "[a-zA-Z0-9\+\-\*\/\=\!]+" |>> Token

  let private plist =
    between <| pstring "("
            <| pstring ")"
            <| (sepEndBy expr spaces) |>> Expr.List

  do exprRef := choice [token
                        plist]

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
