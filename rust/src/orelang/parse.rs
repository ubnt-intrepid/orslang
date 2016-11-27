use std::str;
use nom::{self, multispace};

use super::Error;


#[derive(Debug)]
pub enum Expr {
  Symbol(String),
  Function(String, Vec<Expr>),
}


#[inline]
fn is_tokenchar(c: u8) -> bool {
  match c as char {
    'a'...'z' | 'A'...'Z' | '0'...'9' | '+' | '-' | '*' | '/' | '=' | '!' => true,
    _ => false,
  }
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
      array => { |(s,a)| Expr::Function(s,a) }
    | token => { |s| Expr::Symbol(String::from(s)) }
  )
);

impl Expr {
  pub fn from_str(s: &str) -> Result<Expr, Error> {
    match expr(s.trim().as_bytes()) {
      nom::IResult::Done(_, expr) => Ok(expr),
      _ => return Err(Error::NomParse),
    }
  }
}
