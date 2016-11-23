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

    let re_replace (p : string) (r : string) (i : string) : string = Regex.Replace(i, p, r)

    let rec unwrappedList<'T> (lst : 'T option list) : 'T list option =
        match lst with
        | (Some hd) :: tl -> unwrappedList tl |> Option.map (fun tl -> hd :: tl)
        | None :: _ -> None
        | [] -> Some []

    let rec private exprFromJson json : Expr option =
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

    let ParseFromString(s : string) : Expr option =
        s
        |> (re_replace ";.*" "")
        |> (re_replace @"\(\s*" "[")
        |> (re_replace @"\s*\)" "]")
        |> (re_replace "\n" "")
        |> (re_replace @"^\s+" "")
        |> (re_replace @"\s+$" "")
        |> (re_replace @"\s+" ", ")
        |> (re_replace @"[+*=](?=[, \]])" "\"$0\"")
        |> (re_replace "[a-zA-Z_][a-zA-Z0-9_]*" "\"$0\"")
        |> JsonValue.Parse
        |> exprFromJson

    type Engine() =
        let mutable env = Dictionary<string, Expr>()

        member private this.getValue (k : string) =
            match env.TryGetValue(k) with
            | (true, v) -> Some v
            | _ -> None

        member private this.setValue (k : string) (v : Expr) =
            if env.ContainsKey(k) then env.Remove(k) |> ignore
            env.Add(k, v)

        member private this.substitute (expr : Expr) : Expr option =
            match expr with
            | Expr.Command _ ->
                match this.Evaluate expr with
                | Some(Expr.Value v) -> Some(Expr.Value v)
                | _ -> None
            | Expr.Value v -> Some(Expr.Value v)
            | _ -> None

        member this.Evaluate(expr : Expr) =
            match expr with
            | Expr.Command(symbol, []) -> this.getValue symbol
            | Expr.Command("step", lines) -> this.evalStep lines
            | Expr.Command("until", [ e0; e1 ]) -> this.evalUntil e0 e1
            | Expr.Command("set", [ Expr.String k; v ]) ->
                this.substitute v |> Option.map (fun v ->
                                         this.setValue k v
                                         Expr.Nil)
            | Expr.Command("=", [ lhs; rhs ]) -> this.evalCmp (=) lhs rhs
            | Expr.Command("+", [ lhs; rhs ]) -> this.evalBinOp (+) lhs rhs
            | _ -> None

        member private this.evalStep lines =
            let rec eval_step lines =
                let ret = this.Evaluate(List.head lines)
                match (List.tail lines) with
                | [] -> ret
                | tl -> eval_step tl
            eval_step lines

        member private this.evalUntil pred expr =
            let rec eval_until e0 e1 =
                match this.Evaluate e0 with
                | Some(Expr.Bool true) -> Some Expr.Nil
                | _ ->
                    this.Evaluate e1 |> ignore
                    eval_until e0 e1
            eval_until pred expr

        member private this.evalCmp op lhs rhs =
            match (this.substitute lhs, this.substitute rhs) with
            | (Some(Expr.Value l), Some(Expr.Value r)) -> Some(Expr.Bool(op l r))
            | _ -> None

        member private this.evalBinOp op lhs rhs =
            match (this.substitute lhs, this.substitute rhs) with
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
