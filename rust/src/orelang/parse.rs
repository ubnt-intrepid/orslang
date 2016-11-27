use std::str;
use nom::{self, multispace};

use super::Error;


#[inline]
fn is_tokenchar(c: u8) -> bool {
  match c as char {
    'a'...'z' | 'A'...'Z' | '0'...'9' | '+' | '-' | '*' | '/' | '=' | '!' => true,
    _ => false,
  }
}
named!(tokenchar, take_while_s!(is_tokenchar));


#[derive(Debug)]
pub enum Expr {
  Nil,
  Bool(bool),
  Number(i64),
  Symbol(String),
  Function(String, Vec<Expr>),
}

named!(expr<Expr>, alt!(
     function
   | nil
   | boolean
   | num_or_sym));

named!(nil<Expr>,
  map!(tag!("nil"), |_| Expr::Nil));

named!(boolean<Expr>,
  alt!(
      tag!("true")  => { |_| Expr::Bool(true) }
    | tag!("false") => { |_| Expr::Bool(false) }
  ));

named!(num_or_sym <Expr>,
  map_res!(call!(tokenchar),
  |t| -> Result<Expr, ()> {
    let t = str::from_utf8(t).map_err(|_|())?;
    match t {
      t if t.parse::<i64>().is_ok()
        => Ok(Expr::Number(t.parse::<i64>().unwrap())),
      _ => Ok(Expr::Symbol(t.into())),
    }
  }));

named!(function<Expr>,
  delimited!(
    tag!("("),
    chain!(
        opt!(multispace)
      ~ s: map_res!(call!(tokenchar), str::from_utf8)
      ~ multispace
      ~ a: separated_list!(multispace, expr)
      ~ opt!(multispace)
   , ||{ Expr::Function(s.to_owned(), a) }),
    tag!(")")
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
