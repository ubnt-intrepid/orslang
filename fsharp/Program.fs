// Program.fs

type Result<'t, 'e> = OK of 't | Error of 'e

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
    spaces >>. regex "[a-zA-Z0-9\+\-\*\/\=\!]+" .>> spaces |>> Token

  let private plist expr =
    between <| pstring "(" <| pstring ")" <| sepEndBy expr spaces |>> TList

  let private expr =
    fix <| fun expr -> token <|> plist expr

  type Ast = Nil
           | Symbol of string
           | Number of decimal
           | Command of string * Ast list


  let private unwrappedMap =
    let f acc s = acc |> Option.bind (fun l -> Option.map (fun s -> List.append l [s]) s)
    List.fold f (Some [])

  let rec private toAst (expr: Expr) =
    match expr with
    | Token "nil" -> Some Nil
    | Token s ->
        match System.Decimal.TryParse (s) with
        | (true, d) -> Some (Number d)
        | _         -> Some (Symbol s)
    | TList l ->
        match l.Head with
        | Token command ->
          let args = l.Tail |> List.map toAst |> unwrappedMap
          Option.map (fun args -> Command (command, args)) <| args
        | _ -> None

  let FromString (s: string) : Result<Ast, ErrorKind> =
    match run (spaces >>. expr .>> spaces .>> eof) s with
    | Success (r, _, _)   -> match toAst r with
                             | Some ast -> OK ast
                             | None -> Result.Error <| ParseError "failed to convert to AST"
    | Failure (msg, _, _) -> Result.Error <| ParseError msg

  let FromFile (path: string) : Result<Ast, ErrorKind> =
    FromString <| File.ReadAllText path


[<EntryPoint>]
let main _ =
  let test_string s =
    match Parse.FromString s with
    | Result.OK ast -> printfn "string:\n%A" ast
    | Result.Error msg -> printfn "failed to parse: %A" msg

  let test_file path =
    match Parse.FromFile path with
    | Result.OK ast -> printfn "%s:\n%A" <| System.IO.Path.GetFileName path <| ast
    | Result.Error msg -> printfn "failed to parse: %A" msg

  test_string   "(+ 1 2 (* 3 4))"
  test_string   "(+ 1 2 (* 3 4)))"
  test_file     "../examples/example_sum.ore"
  0
