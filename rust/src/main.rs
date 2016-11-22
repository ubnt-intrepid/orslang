extern crate rustc_serialize;
use rustc_serialize::json::Json;
use std::collections::HashMap;

use std::ops::{Add, Sub, Mul, Div};

struct Evaluator(HashMap<String, f64>);

impl Evaluator {
  fn new() -> Evaluator { Evaluator(HashMap::new()) }

  fn get(&self, key: &str) -> f64 { *self.0.get(key).unwrap() }

  fn set(&mut self, key: &str, val: f64) { self.0.insert(key.to_owned(), val); }

  fn substitute(&mut self, expr: &Json) -> f64 {
    match *expr {
      Json::Array(_) => self.eval(expr).as_f64().unwrap(),
      Json::F64(i) => i,
      Json::I64(i) => i as f64,
      Json::U64(i) => i as f64,
      _ => panic!("cannot substitute: {:?} (env: {:?})", expr, self.0)
    }
  }

  fn eval(&mut self, input: &Json) -> Json {
    let input = input.as_array().expect(r"'input' should be an array").as_slice();

    let token = input[0].as_string().unwrap();
    let args = &input[1..];

    match token {
      "step" => self.eval_step(args).unwrap(),
      "until" => self.eval_until(args),

      "get" => self.eval_get(args),
      "set" => self.eval_set(args),

      "==" => self.eval_cmp(args, |l, r| l == r),
      "!=" => self.eval_cmp(args, |l, r| l != r),
      ">" => self.eval_cmp(args, |l, r| l > r),
      ">=" => self.eval_cmp(args, |l, r| l >= r),
      "<" => self.eval_cmp(args, |l, r| l < r),
      "<=" => self.eval_cmp(args, |l, r| l <= r),

      "+" => self.eval_binop(args, Add::add),
      "-" => self.eval_binop(args, Sub::sub),
      "*" => self.eval_binop(args, Mul::mul),
      "/" => self.eval_binop(args, Div::div),

      _ => panic!("invalid token: {:?}", token)
    }
  }

  fn eval_step(&mut self, lines: &[Json]) -> Result<Json, String> {
    let mut ret = Ok(Json::Null);
    for line in lines {
      ret = Ok(self.eval(&line));
    }
    ret
  }

  fn eval_until(&mut self, args: &[Json]) -> Json {
    loop {
      let c = self.eval(&args[0]);
      if c.is_boolean() && c.as_boolean().unwrap() {
        return Json::Null;
      }
      self.eval(&args[1]);
    }
  }

  fn eval_get(&mut self, args: &[Json]) -> Json {
    let key = args[0].as_string().unwrap();
    Json::F64(self.get(key))
  }

  fn eval_set(&mut self, args: &[Json]) -> Json {
    let key = args[0].as_string().unwrap();
    let val = self.substitute(&args[1]);
    self.set(key, val);
    Json::Null
  }

  fn eval_cmp<C>(&mut self, args: &[Json], cmp: C) -> Json
    where C: Fn(f64, f64) -> bool
  {
    let lhs = self.substitute(&args[0]);
    let rhs = self.substitute(&args[1]);
    Json::Boolean(cmp(lhs, rhs))
  }

  fn eval_binop<O>(&mut self, args: &[Json], op: O) -> Json
    where O: Fn(f64, f64) -> f64
  {
    let lhs = self.substitute(&args[0]);
    let rhs = self.substitute(&args[1]);
    Json::F64(op(lhs, rhs))
  }
}

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
  let input = Json::from_str(source).unwrap();

  let mut e = Evaluator::new();

  println!("{:?}", e.eval(&input));
}
