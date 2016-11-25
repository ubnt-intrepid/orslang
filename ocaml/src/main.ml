(* main.ml *)

let test_str s =
  match Orelang.parse_from_str s with
  | Some result ->
    Printf.printf "%s\n" (Orelang.show_expr result) |> ignore
  | None -> ()

let () =
  test_str "hoge";
  test_str "(set sum 0)";
  test_str "(set sum (+ 1 0))"
