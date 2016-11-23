open System
open FSharp.Data
open System.Collections.Generic

module orelang =
  type Expr = JsonValue

  let transpile_from_string (s: string) =
    JsonValue.Parse s

  type Engine () =
    let mutable env = Dictionary<string, double>()

    member private this.getValue (k: string) =
      match env.TryGetValue(k) with
      | (true, v) -> Some v
      | _         -> None

    member private this.setValue (k: string) (v: double) =
      if env.ContainsKey(k) then
        env.Remove(k) |> ignore
      env.Add(k, v)

    member private this.substitute (expr: Expr) =
      match expr with
      | JsonValue.Array arr ->
        match this.Evaluate expr with
        | Some (JsonValue.Float v)  -> Some v
        | Some (JsonValue.Number v) -> Some (double v)
        | _                         -> None
      | JsonValue.Float v   -> Some v
      | JsonValue.Number v  -> Some (double v)
      | _                   -> None
 
    member this.Evaluate (expr: Expr) =
      let arr: Expr [] = expr.AsArray ()
      let token: string = (Array.head arr).AsString ()
      let args: Expr [] = Array.tail arr
      match token with
      | "step" ->
        let rec eval_step args =
          let ret = this.Evaluate (Array.head args)
          match (Array.tail args) with
          | [||]    -> ret
          | tl      -> eval_step tl
        eval_step args
      | "until" ->
        let rec eval_until e0 e1 =
          match this.Evaluate e0 with
          | Some (JsonValue.Boolean true)   -> Some JsonValue.Null
          | _ -> this.Evaluate e1 |> ignore; eval_until e0 e1
        eval_until (Array.get args 0) (Array.get args 1)
      | "get" ->
        let k = (Array.get args 0).AsString ()
        match this.getValue k with
        | Some v    -> Some (JsonValue.Float v)
        | None      -> None
      | "set" ->
        let k = (Array.get args 0).AsString ()
        let v = this.substitute (Array.get args 1)
        match v with
        | Some v    -> this.setValue k v; Some (JsonValue.Null)
        | None      -> None
      | "==" ->
        let lhs = this.substitute (Array.get args 0)
        let rhs = this.substitute (Array.get args 1)
        match (lhs, rhs) with
        | (Some l, Some r)  -> Some (JsonValue.Boolean (Math.Abs(l - r) < 1e-6))
        | _                 -> None
      | "+" ->
        let lhs = this.substitute (Array.get args 0)
        let rhs = this.substitute (Array.get args 1)
        match (lhs, rhs) with
        | (Some l, Some r)  -> Some (JsonValue.Float (l + r))
        | _                 -> None
      | _ -> None


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
  let parsed = orelang.transpile_from_string source
  let e = new orelang.Engine ()
  match e.Evaluate parsed with
  | Some value -> Console.WriteLine("result: {0}", value)
  | None       -> Console.WriteLine("failed to parse sources")
  0
