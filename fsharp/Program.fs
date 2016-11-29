// Program.fs
open System.IO
open System.Collections.Generic

type Result<'t, 'e> =
    | OK of 't
    | Error of 'e

let rec fix f x = f (fix f) x

type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x

let maybe = new MaybeBuilder()
let (>>=) x f = x |> Option.bind f

type expr =
    | Nil
    | Boolean of bool
    | Number of int
    | Symbol of string
    | Call of string * expr list
    | Operator of operator_t
    | Lambda of string list * expr

and operator_t = delegate of expr list -> expr option

module Expr =
    let IsNil = function
        | Nil -> true
        | _ -> false

    let ToBoolean = function
        | Boolean b -> Some b
        | _ -> None

    let ToNumber = function
        | Number d -> Some d
        | _ -> None

    let ToSymbol = function
        | Symbol s -> Some s
        | _ -> None

module Parser =
    open System
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers

    let private nil = pstring "nil" >>% Nil
    let private number = regex "[+-]?[0-9]+" |>> (Int32.Parse >> Number)
    let private symbol = regex "[a-zA-Z0-9\+\-\*\/\=\!]+"
    let private token = spaces >>. (nil <|> number <|> (symbol |>> Symbol)) .>> spaces

    let private command expr =
        pstring "(" >>. spaces >>. symbol .>> spaces .>>. expr .>> spaces .>>. sepEndBy expr spaces
        .>> pstring ")" >>= fun ((op, arg), args) ->
            // TODO: simplify
            match op with
            | "lambda" ->
                match arg with
                | Call(hd, tl) ->
                    let r =
                        maybe {
                            let! tl = tl
                                      |> List.map Expr.ToSymbol
                                      |> List.fold
                                             (fun acc x -> maybe { let! acc = acc
                                                                   let! x = x
                                                                   return List.append acc [ x ] })
                                             (Some [])
                            let! a = args |> List.tryHead
                            return Lambda(hd :: tl, a)
                        }
                    try
                        preturn r.Value
                    with _ -> fail "failed to parse lambda"
                | _ -> fail "lambda: the first argument is not a list of symbols"
            | _ -> preturn (Call(op, arg :: args))

    let private expr = fix <| fun expr -> token <|> command expr

    let ParseFromString s =
        let parser = spaces >>. expr .>> spaces .>> eof
        match run parser s with
        | Success(r, _, _) -> Result.OK r
        | Failure(msg, _, _) -> Result.Error msg

    let ParseFromFile path =
        ParseFromString <| System.IO.File.ReadAllText path

type engine() =
    let env = new Dictionary<string, expr>()

    member this.Default() =
        let defineOperator name f = env.Add(name, Operator(operator_t (f)))
        defineOperator "set" <| fun args ->
            maybe {
                let! k = List.tryItem 0 args >>= Expr.ToSymbol
                let! v = List.tryItem 1 args >>= this.Evaluate
                this.setValue k v
                return Nil
            }
        defineOperator "until" <| fun args ->
            let rec f pred expr =
                match this.Evaluate pred >>= Expr.ToBoolean with
                | Some true -> ()
                | _ ->
                    this.Evaluate expr |> ignore
                    f pred expr
            maybe {
                let! pred = List.tryItem 0 args
                let! expr = List.tryItem 1 args
                f pred expr
                return Nil
            }
        defineOperator "step" <| fun args ->
            let rec eval_step lines =
                let ret = this.Evaluate(List.head lines)
                match (List.tail lines) with
                | [] -> ret
                | tl -> eval_step tl
            eval_step args
        defineOperator "print" <| fun args ->
            maybe {
                let! arg = List.tryHead args >>= this.Evaluate
                printfn "%A" arg
                return Nil
            }
        defineOperator "quote" <| fun args -> List.tryHead args
        defineOperator "+" <| fun args ->
            args
            |> List.fold (fun acc x ->
                   maybe {
                       let! acc = acc
                       let! x = x
                                |> this.Evaluate
                                >>= Expr.ToNumber
                       return acc + x
                   }) (Some 0)
            |> Option.map Number
        defineOperator "*" <| fun args ->
            args
            |> List.fold (fun acc x ->
                   maybe {
                       let! acc = acc
                       let! x = x
                                |> this.Evaluate
                                >>= Expr.ToNumber
                       return acc * x
                   }) (Some 1)
            |> Option.map Number
        defineOperator "=" <| fun args ->
            maybe {
                let! lhs = List.tryItem 0 args
                           >>= this.Evaluate
                           >>= Expr.ToNumber
                let! rhs = List.tryItem 1 args
                           >>= this.Evaluate
                           >>= Expr.ToNumber
                return Boolean(lhs = rhs)
            }
        this

    member private this.getValue k =
        match env.TryGetValue(k) with
        | (true, v) -> Some v
        | _ -> None

    member private this.setValue k v =
        env.Remove(k) |> ignore
        env.Add(k, v)

    member this.evalFunc op args =
        this.getValue op >>= function
            | Symbol op         -> this.evalFunc op args
            | Lambda(syn, expr) -> this.evalLambda syn expr args
            | Operator op       -> op.Invoke(args)
            | _                 -> None

    member this.evalLambda syn expr args =
        let local_eng = engine()
        // FIX: use local environment
        for k, v in List.zip syn args do
            this.setValue k v
        this.evaluate expr local_eng

    member this.evaluate expr local = this.Evaluate expr

    member this.Evaluate(expr : expr) : expr option =
        match expr with
        | Nil | Boolean _ | Number _ -> Some expr
        | Lambda _ -> Some expr
        | Symbol k -> this.getValue k
        | Call(name, args) -> this.evalFunc name args
        | Operator _ -> Some expr

    member this.print() = printfn "env: %A" env

let testEval expr =
    match expr with
    | Result.Error msg -> printfn "failed to parse: %A\n" msg
    | Result.OK expr ->
        let engine = engine().Default()
        printfn "result: %A" <| engine.Evaluate expr

let testString s =
    printfn "\nstring: %s" s
    testEval <| Parser.ParseFromString s

let testFile filename =
    printfn "\nfile: %s" filename
    let repo_root = Directory.GetParent(__SOURCE_DIRECTORY__).ToString()
    testEval (Parser.ParseFromFile <| repo_root + "/examples/" + filename)

[<EntryPoint>]
let main _ =
    testString "nil"
    testString "(+ 1 2 (* 3 4))"
    testString "(set i 10)"
    testString "(until (= 1 1) (set i 10))"
    testString "(step (set i (quote hoge)) i)"
    testString "(step (set i (quote (+ 1 2 3))) (print i))"
    testFile "example_sum.ore"
    testFile "example_firstclass_op.ore"
    testFile "example_lambda.ore"
    // failure case
    // test_string   "(+ 1 2 (* 3 4)))"
    0
