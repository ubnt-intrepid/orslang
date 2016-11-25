use std::fs::File;
use std::io::Read;
use std::str;
use nom::{self, is_alphanumeric, multispace};

#[derive(Debug)]
pub enum Expr {
  Token(String),
  Array(Vec<Expr>),
}

#[inline]
fn is_tokenchar(c: u8) -> bool {
  is_alphanumeric(c) || c == '+' as u8 || c == '-' as u8 || c == '*' as u8 || c == '/' as u8 ||
  c == '=' as u8 || c == '!' as u8
}

named!(tokenchar, take_while_s!(is_tokenchar));

named!(token<&str>, map_res!(call!(tokenchar), str::from_utf8));

named!(array< Vec<Expr> >,
  delimited!(
    tag!("("),
    chain!(e:separated_list!(multispace, expr) ~ opt!(multispace), ||{ e }),
    tag!(")")
  )
);

named!(expr<Expr>,
  alt!(
      array => { |v| Expr::Array(v)               }
    | token => { |s| Expr::Token(String::from(s)) }
  )
);

pub fn from_str(s: &str) -> Result<Expr, super::Error> {
  match expr(s.trim().as_bytes()) {
    nom::IResult::Done(_, expr) => Ok(expr),
    _ => Err(super::Error::NomParse),
  }
}

pub fn from_file(path: &str) -> Result<Expr, super::Error> {
  let mut content = String::new();
  File::open(path)?.read_to_string(&mut content)?;
  from_str(&content)
}

impl super::ast::IntoAst for Expr {
  fn into_ast(self) -> Result<super::ast::Ast, super::Error> {
    Err(super::Error::BuildAst)
  }
}
