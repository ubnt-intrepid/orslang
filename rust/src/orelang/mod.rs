use std::io::{self, Read};
use std::fs::File;

mod parse;

pub use self::parse::Ast;

#[derive(Debug)]
pub enum Error {
  Io(io::Error),
  NomParse,
  BuildAst,
}

impl From<io::Error> for Error {
  fn from(err: io::Error) -> Error {
    Error::Io(err)
  }
}

pub fn parse_from_str(s: &str) -> Result<Ast, Error> {
  Ast::from_str(s)
}

pub fn parse_from_file(path: &str) -> Result<Ast, Error> {
  let mut content = String::new();
  File::open(path)?.read_to_string(&mut content)?;
  parse_from_str(&content)
}
