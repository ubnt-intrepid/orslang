use std::io::{self, Read};
use std::fs::File;

mod parse;

pub use self::parse::Expr;

#[derive(Debug)]
pub enum Error {
  Io(io::Error),
  NomParse,
}

impl From<io::Error> for Error {
  fn from(err: io::Error) -> Error {
    Error::Io(err)
  }
}

pub fn parse_from_str(s: &str) -> Result<Expr, Error> {
  Expr::from_str(s)
}

pub fn parse_from_file(path: &str) -> Result<Expr, Error> {
  let mut content = String::new();
  File::open(path)?.read_to_string(&mut content)?;
  parse_from_str(&content)
}
