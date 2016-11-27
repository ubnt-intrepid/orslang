use std::str;
use nom::{self, is_alphanumeric, multispace};

use super::Error;

#[inline]
fn is_tokenchar(c: u8) -> bool {
  is_alphanumeric(c) || c == '+' as u8 || c == '-' as u8 || c == '*' as u8 || c == '/' as u8 ||
  c == '=' as u8 || c == '!' as u8
}

named!(tokenchar, take_while_s!(is_tokenchar));

named!(token<&str>, map_res!(call!(tokenchar), str::from_utf8));

named!(array< (String, Vec<Expr>) >,
  delimited!(
    tag!("("),
    chain!(
        opt!(multispace)
      ~ s: map_res!(call!(tokenchar), str::from_utf8)
      ~ multispace
      ~ a: separated_list!(multispace, expr)
      ~ opt!(multispace)
   , ||{ (s.to_owned(), a) }),
    tag!(")")
  )
);

named!(expr<Expr>,
  alt!(
      array => { |(s,a)| Expr::Array(s,a)               }
    | token => { |s| Expr::Token(String::from(s)) }
  )
);


#[derive(Debug)]
enum Expr {
  Token(String),
  Array(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Ast {
  Nil,
  Symbol(String),
  Number(i64),
  Command(String, Vec<Ast>),
}

impl Ast {
  pub fn from_str(s: &str) -> Result<Ast, Error> {
    match expr(s.trim().as_bytes()) {
      nom::IResult::Done(_, expr) => Ok(expr.into()),
      _ => return Err(Error::NomParse),
    }
  }
}

impl Into<Ast> for Expr {
  fn into(self) -> Ast {
    match self {
      Expr::Token(ref s) if s.trim() == "nil" => Ast::Nil,
      Expr::Token(s) => s.parse::<i64>().map(Ast::Number).unwrap_or(Ast::Symbol(s)),
      Expr::Array(s, arr) => Ast::Command(s, arr.into_iter().map(Into::into).collect()),
    }
  }
}
