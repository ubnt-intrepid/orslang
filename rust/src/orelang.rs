use std::collections::HashMap;
use std::ops::Add;

use rustc_serialize::json::Json;

type Expr = Json;

pub fn transpile_from_str(s: &str) -> Expr {
  Json::from_str(s).unwrap()
}


pub struct Engine {
  env: HashMap<String, f64>,
}

impl Engine {
  pub fn new() -> Engine {
    Engine { env: HashMap::new() }
  }

  fn get(&self, key: &str) -> f64 {
    *self.env.get(key).unwrap()
  }

  fn set(&mut self, key: &str, val: f64) {
    self.env.insert(key.to_owned(), val);
  }

  fn substitute(&mut self, expr: &Json) -> f64 {
    match *expr {
      Json::Array(_) => self.eval(expr).as_f64().unwrap(),
      Json::F64(i) => i,
      Json::I64(i) => i as f64,
      Json::U64(i) => i as f64,
      _ => panic!("cannot substitute: {:?} (env: {:?})", expr, self.env),
    }
  }

  pub fn eval(&mut self, input: &Json) -> Json {
    let input = input.as_array().expect(r"'input' should be an array").as_slice();

    let token = input[0].as_string().unwrap();
    let args = &input[1..];

    match token {
      "step" => self.eval_step(args).unwrap(),
      "until" => self.eval_until(args),
      "get" => self.eval_get(args),
      "set" => self.eval_set(args),
      "==" => self.eval_cmp(args, |l, r| l == r),
      "+" => self.eval_binop(args, Add::add),

      _ => panic!("invalid token: {:?}", token),
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
