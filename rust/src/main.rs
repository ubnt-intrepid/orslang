extern crate rustc_serialize;

mod orelang;

fn main() {
  let source = r#"
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
"#;

  let input = orelang::transpile_from_str(source);

  let mut e = orelang::Engine::new();
  println!("{:?}", e.eval(&input));
}
