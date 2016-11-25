(* main.ml *)

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let test_str s =
  match Orelang.parse_from_str s with
  | Some result ->
    Printf.printf "%s\n" (Orelang.show_expr result) |> ignore
  | None -> ()

let test_file filename =
  load_file filename |> test_str

let () =
  test_str "hoge";
  test_str "(set sum 0)";
  test_str "(set sum (+ 1 0 ) ) ";

  test_file "../examples/example_sum.ore"
