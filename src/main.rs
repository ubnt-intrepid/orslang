extern crate rustc_serialize;
use rustc_serialize::json::Json;
use std::collections::HashMap;


struct Evaluator(HashMap<String, i64>);

impl Evaluator {
  fn new() -> Evaluator { Evaluator(HashMap::new()) }

  fn eval(&mut self, input: &[Json]) -> Json {
    match input[0] {
      Json::String(ref s) => {
        match s.as_str() {
          "step" => self.eval_step(&input[1..]),
          "until" => self.eval_until(&input[1..]),
          "get" => self.eval_get(&input[1..]),
          "set" => self.eval_set(&input[1..]),
          "=" => self.eval_equals(&input[1..]),
          "+" => self.eval_add(&input[1..]),
          _ => panic!("invalid operator: {:?}", s)
        }
      }
      _ => panic!("invalid token: {:?}", input[0])
    }
  }

  fn eval_step(&mut self, lines: &[Json]) -> Json {
    let ret = self.eval(lines[0].as_array().unwrap().as_slice());
    if lines.len() == 1 {
      ret
    } else {
      self.eval_step(&lines[1..])
    }
  }

  #[allow(unreachable_code)]
  fn eval_until(&mut self, args: &[Json]) -> Json {
    loop {
      let c = self.eval(args[0].as_array().unwrap().as_slice());
      if c.is_boolean() && c.as_boolean().unwrap() {
        return Json::Null;
      }
      self.eval(args[1].as_array().unwrap().as_slice());
    }
  }

  fn eval_get(&mut self, args: &[Json]) -> Json {
    let key = args[0].as_string().unwrap();
    self.0.get(key).cloned().map(Json::I64).unwrap()
  }

  fn eval_set(&mut self, args: &[Json]) -> Json {
    let key = args[0].as_string().unwrap();
    let val = self.substitute(&args[1]);
    *self.0.entry(key.to_owned()).or_insert(0) = val;
    Json::Null
  }

  fn eval_equals(&mut self, args: &[Json]) -> Json {
    let lhs = self.substitute(&args[0]);
    let rhs = self.substitute(&args[1]);
    Json::Boolean(lhs == rhs)
  }

  fn eval_add(&mut self, args: &[Json]) -> Json {
    let lhs = self.substitute(&args[0]);
    let rhs = self.substitute(&args[1]);
    Json::I64(lhs + rhs)
  }

  fn substitute(&mut self, expr: &Json) -> i64 {
    match *expr {
      Json::Array(ref arr) => self.eval(arr.as_slice()).as_i64().unwrap(),
      Json::I64(ref i) => *i,
      Json::U64(ref i) => *i as i64,
      _ => panic!("cannot substitute: {:?} (env: {:?})", expr, self.0)
    }
  }
}


fn main() {
  let source = r#"
["step",
  ["set", "i", 10],
  ["set", "sum", 0],
  ["until", ["=", ["get", "i"], 0], [
    "step",
    ["set", "sum", ["+", ["get", "sum"], ["get", "i"]]],
    ["set", "i", ["+", ["get", "i"], -1]]
  ]],
  ["get", "sum"]
]
"#;

  let input: Vec<Json> = Json::from_str(source).unwrap().as_array().unwrap().to_owned();
  println!("{:?}", Evaluator::new().eval(input.as_slice()));
}
