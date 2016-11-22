open System
open FSharp.Data

type Dictionary<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>
type Expr = JsonValue

type Evaluator () =
  let mutable env = Dictionary<string, double>()

  member private this.substitute (expr: Expr) =
    None

  member private this.getVar (k: string) =
    match env.TryGetValue(k) with
    | (true, v) -> Some v
    | _         -> None

  member private this.setVar (k: string) (v: double) =
    if env.ContainsKey(k) then
      env.Remove(k) |> ignore
    env.Add(k, v)

  member this.Evaluate (expr: Expr) =
    this.setVar "hoge" 10.0
    this.getVar "hoge" |> Console.WriteLine
    this.setVar "hoge" 20.0
    this.getVar "hoge" |> Console.WriteLine
    None


let source = """
["step",
  ["set", "i", 10],
  ["set", "sum", 0],
  ["until", ["==", ["get", "i"], 0], [
    "step",
    ["set", "sum", ["+", ["get", "sum"], ["get", "i"]]],
    ["set", "i", ["+", ["get", "i"], -1]]
  ]],
  ["get", "sum"]
]
"""

[<EntryPoint>]
let main _ =
    let parsed = JsonValue.Parse source
    let e = new Evaluator ()
    match e.Evaluate parsed with
    | Some value -> Console.WriteLine("result: {0}", value)
    | None       -> Console.WriteLine("failed to parse sources")
    0
