extern crate rustc_serialize;
use rustc_serialize::json::Json;
use std::collections::HashMap;

fn eval(input: &[Json]) -> Json {
  let mut env = std::default::Default::default();
  do_run(input, &mut env)
}

fn substitute(expr: &Json, env: &mut HashMap<String, i64>) -> i64 {
  match *expr {
    Json::Array(ref arr) => do_run(arr.as_slice(), env).as_i64().unwrap(),
    Json::I64(ref i) => *i,
    Json::U64(ref i) => *i as i64,
    _ => panic!("cannot substitute: {:?} (env: {:?})", expr, env)
  }
}

#[allow(unreachable_code)]
fn do_run(input: &[Json], env: &mut HashMap<String, i64>) -> Json {
  match input[0] {
    Json::String(ref s) => {
      match s.as_str() {
        "step" => {
          let ret: Vec<_> = (&input[1..]).iter().map(|line| do_run(line.as_array().unwrap().as_slice(), env)).collect();
          ret.into_iter().last().unwrap()
        }
        "until" => {
          loop {
            let c = do_run(input[1].as_array().unwrap().as_slice(), env);
            if c.is_boolean() && c.as_boolean().unwrap() {
              return Json::Null;
            }
            do_run(input[2].as_array().unwrap().as_slice(), env);
          }
          unreachable!()
        }
        "get" => {
          let key = input[1].as_string().unwrap();
          env.get(key).cloned().map(Json::I64).unwrap()
        }
        "set" => {
          let key = input[1].as_string().unwrap();
          let val = substitute(&input[2], env);
          *env.entry(key.to_owned()).or_insert(0) = val;
          Json::Null
        }
        "=" => {
          let lhs = substitute(&input[1], env);
          let rhs = substitute(&input[2], env);
          Json::Boolean(lhs == rhs)
        }
        "+" => {
          let lhs = substitute(&input[1], env);
          let rhs = substitute(&input[2], env);
          Json::I64(lhs + rhs)
        }
        _ => panic!("invalid operator: {:?}", s)
      }
    }
    _ => panic!("invalid token: {:?}", input[0])
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
  println!("{:?}", eval(input.as_slice()));
}
