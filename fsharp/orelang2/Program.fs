type ('t, 'e) Result = OK of 't | Error of 'e

module Parse =
  open FParsec
  open FParsec.Primitives
  open FParsec.CharParsers
  open System.IO

  let FromString (s: string) : Result<string, string> =
    Result.Error "not implemented"

  let FromFile (path: string) : Result<string, string> =
    FromString <| File.ReadAllText path

[<EntryPoint>]
let main _ =
  let test_string s =
    printfn "simple: %A" <| Parse.FromString s

  let test_file path =
    printfn "%s: %A" <| System.IO.Path.GetFileName path <| Parse.FromFile path

  test_string   "(+ 1 2 (* 3 4))"
  test_file     "example_sum.ore"
  0
