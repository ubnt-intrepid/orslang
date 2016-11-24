extern crate rustc_serialize;
#[macro_use]
extern crate nom;

mod orelang;

mod parse {
  use std::fs::File;
  use std::io::{self, Read};
  use std::str;
  use nom::{self, alphanumeric};

  #[derive(Debug)]
  pub enum Error {
    Io(io::Error),
    Unknown,
  }

  impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
      Error::Io(err)
    }
  }


  #[derive(Debug)]
  pub enum Expr {
    Token(String),
    Array(Vec<Expr>),
  }

  named!(token<&str>, map_res!(call!(alphanumeric),str::from_utf8));

  named!(array< Vec<Expr> >,
    delimited!(
      tag!("("),
      separated_list!(tag!(" "), expr),
      tag!(")")
  )
);

  named!(expr<Expr>,
    alt!(
      array => { |v| Expr::Array(v)               }
    | token => { |s| Expr::Token(String::from(s)) }
    )
  );

  pub fn from_str(s: &str) -> Result<Expr, Error> {
    match expr(s.as_bytes()) {
      nom::IResult::Done(_, expr) => Ok(expr),
      _ => Err(Error::Unknown),
    }
  }

  pub fn from_file(path: &str) -> Result<Expr, Error> {
    let mut content = String::new();
    File::open(path)?.read_to_string(&mut content)?;
    from_str(&content)
  }
}


fn main() {
  println!("{:?}", parse::from_str("hoge"));
  println!("{:?}", parse::from_str("(hoge 1 2 (aa 2 3))"));
  println!("{:?}", parse::from_file("../examples/example_sum.ore"));

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
