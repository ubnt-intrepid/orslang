use std::io;

mod parse;
mod ast;

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

pub fn parse_from_str(s: &str) -> Result<ast::Ast, Error> {
  ast::from_expr(parse::from_str(s)?)
}

pub fn parse_from_file(path: &str) -> Result<ast::Ast, Error> {
  ast::from_expr(parse::from_file(path)?)
}