open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Data

module Orelang =
    type Expr =
        | Nil
        | Bool of bool
        | Value of decimal
        | String of string
        | Command of string * Expr list

    let ReReplace (p : string) (r : string) (i : string) = Regex.Replace(i, p, r)

    let rec private unwrappedList lst =
        match lst with
        | (Some hd) :: tl -> unwrappedList tl |> Option.map (fun tl -> hd :: tl)
        | None :: _ -> None
        | [] -> Some []

    let rec private exprFromJson json =
        match json with
        | JsonValue.Record _ -> None
        | JsonValue.Null -> Some Expr.Nil
        | JsonValue.Boolean b -> Some(Expr.Bool b)
        | JsonValue.Number n -> Some(Expr.Value n)
        | JsonValue.Float f -> Some(Expr.Value(decimal f))
        | JsonValue.String s -> Some(Expr.String s)
        | JsonValue.Array a ->
            a
            |> Array.map exprFromJson
            |> Array.toList
            |> unwrappedList
            |> Option.bind (fun lst ->
                   match lst with
                   | (Expr.String s) :: tl -> Some(Expr.Command(s, tl))
                   | _ -> None)

    /// Parse a string to Expr.
    let ParseFromString(s : string) : Expr option =
        s
        |> (ReReplace ";.*" "")
        |> (ReReplace @"\(\s*" "[")
        |> (ReReplace @"\s*\)" "]")
        |> (ReReplace "\n" "")
        |> (ReReplace @"^\s+" "")
        |> (ReReplace @"\s+$" "")
        |> (ReReplace @"\s+" ", ")
        |> (ReReplace @"[+*=](?=[, \]])" "\"$0\"")
        |> (ReReplace "[a-zA-Z_][a-zA-Z0-9_]*" "\"$0\"")
        |> JsonValue.Parse
        |> exprFromJson

    type Engine() =
        let mutable env = Dictionary<string, Expr>()

        member private this.GetValue k =
            match env.TryGetValue(k) with
            | (true, v) -> Some v
            | _ -> None

        member private this.SetValue k v =
            if env.ContainsKey(k) then env.Remove(k) |> ignore
            env.Add(k, v)

        member private this.Substitute expr =
            match expr with
            | Expr.Command _ ->
                match this.Evaluate expr with
                | Some(Expr.Value v) -> Some(Expr.Value v)
                | _ -> None
            | Expr.Value v -> Some(Expr.Value v)
            | _ -> None

        member this.Evaluate(expr : Expr) : Expr option =
            match expr with
            | Expr.Command(symbol, []) -> this.GetValue symbol
            | Expr.Command("set", [ Expr.String k; v ]) ->
              this.Substitute v |> Option.map (fun v -> this.SetValue k v; Expr.Nil)
            | Expr.Command("step", lines) ->
              lines |> List.fold (fun acc line -> this.Evaluate line) None
            | Expr.Command("until", [ e0; e1 ]) -> this.EvalUntil e0 e1
            | Expr.Command("=", [ lhs; rhs ]) -> this.EvalCmp (=) lhs rhs
            | Expr.Command("+", [ lhs; rhs ]) -> this.EvalBinOp (+) lhs rhs
            | _ -> None

        member private this.EvalUntil pred expr =
            let EvalBool pred = match this.Evaluate pred with | Some(Expr.Bool b) -> b | _ -> false
            while not (EvalBool pred) do
                this.Evaluate expr |> ignore
            done
            Some (Expr.Nil)

        member private this.EvalCmp op lhs rhs =
            match (this.Substitute lhs, this.Substitute rhs) with
            | (Some(Expr.Value l), Some(Expr.Value r)) -> Some(Expr.Bool(op l r))
            | _ -> None

        member private this.EvalBinOp op lhs rhs =
            match (this.Substitute lhs, this.Substitute rhs) with
            | (Some(Expr.Value l), Some(Expr.Value r)) -> Some(Expr.Value(op l r))
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
    let eng = new Orelang.Engine()
    match Orelang.ParseFromString source with
    | None -> Console.WriteLine("failed to parse sources")
    | Some ast ->
        match eng.Evaluate ast with
        | Some(Orelang.Expr.Value v) -> Console.WriteLine("computational result = {0}", v)
        | Some value -> Console.WriteLine("computation was finished but the result isn't available.")
        | None -> Console.WriteLine("failed to complete evaluation.")
    0
