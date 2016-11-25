extern crate rustc_serialize;
#[macro_use]
extern crate nom;

mod parse {
  use std::fs::File;
  use std::io::{self, Read};
  use std::str;
  use nom::{self, alphanumeric, multispace};

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
      chain!(
          e: separated_list!(multispace, expr)
        ~ opt!(multispace),
      ||{e}),
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
    match expr(s.trim().as_bytes()) {
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
}
