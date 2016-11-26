use std::str;
use nom::{self, is_alphanumeric, multispace};

use super::Error;


#[derive(Debug)]
enum Expr {
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
    chain!(opt!(multispace) ~ e:separated_list!(multispace, expr) ~ opt!(multispace), ||{ e }),
    tag!(")")
  )
);

named!(expr<Expr>,
  alt!(
      array => { |v| Expr::Array(v)               }
    | token => { |s| Expr::Token(String::from(s)) }
  )
);


#[derive(Debug)]
pub enum Ast {
  Nil,
  Symbol(String),
  Number(i64),
  Command(String, Vec<Ast>),
}

impl Ast {
  pub fn from_str(s: &str) -> Result<Ast, Error> {
    let expr = match expr(s.trim().as_bytes()) {
      nom::IResult::Done(_, expr) => expr,
      _ => return Err(Error::NomParse),
    };
    expr_to_ast(&expr)
  }
}

fn expr_to_ast(expr: &Expr) -> Result<Ast, Error> {
  match *expr {
    Expr::Token(ref s) if s.trim() == "nil" => Ok(Ast::Nil),
    Expr::Token(ref s) => {
      Ok(s.parse::<i64>().map(Ast::Number).unwrap_or(Ast::Symbol((*s).clone())))
    }
    Expr::Array(ref arr) if arr.len() > 1 => {
      match arr[0] {
        Expr::Token(ref s) => {
          let mut args = Vec::with_capacity(arr.len() - 1);
          for arg in &arr[1..] {
            let arg = expr_to_ast(arg)?;
            args.push(arg);
          }
          Ok(Ast::Command(s.to_owned(), args))
        }
        _ => Err(Error::BuildAst),
      }
    }
    _ => Err(Error::BuildAst),
  }
}
