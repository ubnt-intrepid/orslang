open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Data

module Orelang =
  type Expr = JsonValue

  let re_replace (p: string) (r: string) (i: string) : string =
    Regex.Replace (i, p, r)

  let ParseFromString (s: string) : Expr option =
    s |> (re_replace  ";.*"                     "")
      |> (re_replace @"\(\s*"                   "[")
      |> (re_replace @"\s*\)"                   "]")
      |> (re_replace  "\n"                      "")
      |> (re_replace @"^\s+"                    "")
      |> (re_replace @"\s+$"                    "")
      |> (re_replace @"\s+"                     ", ")
      |> (re_replace @"[+*=](?=[, \]])"         "\"$0\"")
      |> (re_replace  "[a-zA-Z_][a-zA-Z0-9_]*"  "\"$0\"")
      |> JsonValue.Parse
      |> Some

  type Engine () =
    let mutable env = Dictionary<string, Expr>()

    member private this.getValue (k: string) =
      match env.TryGetValue(k) with
      | (true, v) -> Some v
      | _         -> None

    member private this.setValue (k: string) (v: Expr) =
      if env.ContainsKey(k) then
        env.Remove(k) |> ignore
      env.Add(k, v)

    member private this.substitute (expr: Expr) =
      match expr with
      | Expr.Array arr ->
        match this.Evaluate expr with
        | Some (Expr.Float v)  -> Some (Expr.Float v)
        | Some (Expr.Number v) -> Some (Expr.Float (double v))
        | _                    -> None
      | Expr.Float v   -> Some (Expr.Float v)
      | Expr.Number v  -> Some (Expr.Float (double v))
      | _              -> None

    member this.Evaluate (expr: Expr) =
      let arr = expr.AsArray ()
      let token = (Array.head arr).AsString ()
      let args = Array.tail arr
      if args.Length = 0 then
        this.getValue token
      else
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
            | Some (Expr.Boolean true) -> Some Expr.Null
            | _ -> this.Evaluate e1 |> ignore
                   eval_until e0 e1
          eval_until (Array.get args 0) (Array.get args 1)

        | "set" ->
          let k = (Array.get args 0).AsString ()
          this.substitute (Array.get args 1) |> Option.map (fun v -> this.setValue k v; Expr.Null)

        | "=" ->
          let lhs = this.substitute (Array.get args 0)
          let rhs = this.substitute (Array.get args 1)
          match (lhs, rhs) with
          | (Some (Expr.Float l), Some (Expr.Float r)) -> Some (Expr.Boolean (Math.Abs(l - r) < 1e-6))
          | _                                          -> None

        | "+" ->
          let lhs = this.substitute (Array.get args 0)
          let rhs = this.substitute (Array.get args 1)
          match (lhs, rhs) with
          | (Some (Expr.Float l), Some (Expr.Float r)) -> Some (Expr.Float (l + r))
          | _                                          -> None

        | _ -> None


[<EntryPoint>]
let main _ =
  let source = """
  (step
    (set i 10)
    (set sum 0)
    (until (= (i) 0)
      (step
        (set sum (+ (sum) (i)))
        (set i (+ (i) -1))))
    (sum)
  )
  """

  let eng = new Orelang.Engine ()

  match Orelang.ParseFromString source with
  | None         -> Console.WriteLine("failed to parse sources")
  | Some ast ->
    match eng.Evaluate ast with
    | Some value -> Console.WriteLine("computational result = {0}", value)
    | None       -> Console.WriteLine("failed to complete evaluation.")
  0
