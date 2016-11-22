(* main.ml *)

module J = Yojson.Basic

type expr = Yojson.Basic.json

let from_string s = J.from_string s

class evaluator =
  object (self)
    val mutable the_env = ( Hashtbl.create 123456 )

    (* get the value of variable from environment *)
    method get_var (k: string) =
      Hashtbl.find the_env k

    (* set the value of variable *)
    method set_var (k: string) (v: float) =
      Hashtbl.replace the_env k v

    method substitute (e: expr) =
      match e with
      | `List (arr) ->
        (match self#eval e with
         | Some `Float v  -> Some v
         | Some `Int v    -> Some (float_of_int v)
         | _              -> None
        )
      | `Float (v)    -> Some v
      | `Int (v)      -> Some (float_of_int v)
      | _             -> None

    method eval (s: expr) =
      let arr = J.Util.to_list s in
      let token = J.Util.to_string (List.hd arr) in
      let args = List.tl arr in
      match token with
      | "step" -> (
          let rec eval_step lines = (
            let ret = self#eval (List.hd lines) in
            match (List.tl lines) with
            | []  -> ret
            | tl  -> eval_step tl
          ) in
          eval_step args
        )

      | "until" -> (
          let rec eval_until e0 e1 = (
            let pred = self#eval e0 in
            match pred with
            | Some `Bool c when c = true -> Some `Null
            | _ -> self#eval e1 |> ignore; eval_until e0 e1
          ) in
          eval_until (List.nth args 0) (List.nth args 1)
        )

      | "get" -> (
          let k = J.Util.to_string (List.hd args) in
          Some (`Float (self#get_var k))
        )

      | "set" -> (
          let k = J.Util.to_string (List.hd args) in
          let v = self#substitute (List.nth args 1) in
          match v with
          | Some v  -> self#set_var k v; Some `Null
          | None    -> None
        )

      | "==" -> (
          let lhs = self#substitute (List.nth args 0) in
          let rhs = self#substitute (List.nth args 1) in
          match (lhs, rhs) with
          | (Some l, Some r)  -> Some (`Bool ((abs_float (l -. r)) < 1e-6))
          | _                 -> None
        )

      | "+" -> (
          let lhs = self#substitute (List.nth args 0) in
          let rhs = self#substitute (List.nth args 1) in
          match (lhs, rhs) with
          | (Some l, Some r)  -> Some (`Float (l +. r))
          | _                 -> None
        )

      | _ -> (None : expr option)
  end


let source = "
 [\"step\",
     [\"set\", \"i\", 10.0],
     [\"set\", \"sum\", 0.0],
     [\"until\", [\"==\", [\"get\", \"i\"], 0.0], [
         \"step\",
         [\"set\", \"sum\", [\"+\", [\"get\", \"sum\"], [\"get\", \"i\"]]],
         [\"set\", \"i\",   [\"+\", [\"get\", \"i\"], -1.0]]
     ]],
     [\"get\", \"sum\"]
 ]
 "

let () =
  let parsed = from_string source in
  let e = new evaluator in
  match (e#eval parsed) with
  | Some v  -> Printf.printf "%s" (J.to_string v)
  | None    -> Printf.printf "failed to evaluate"
