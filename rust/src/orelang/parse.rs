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


#[derive(Debug)]
pub enum Ast {
  Nil,
  Symbol(String),
  Number(i64),
  Step(Vec<Box<Ast>>),
  Set(String, Box<Ast>),
  Until(Box<Ast>, Box<Ast>),
  Print(Box<Ast>),
  Eq(Box<Ast>, Box<Ast>),
  Plus(Box<Ast>, Box<Ast>),
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
          let s: &str = s;
          match s {
            "step" => {
              let mut lines = Vec::with_capacity(arr.len() - 1);
              for line in &arr[1..] {
                let line = expr_to_ast(line)?;
                lines.push(Box::new(line));
              }
              Ok(Ast::Step(lines))
            }
            "set" => {
              let symbol: Ast = arr.iter().nth(1).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              let value: Ast = arr.iter().nth(2).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              if let Ast::Symbol(s) = symbol {
                Ok(Ast::Set(s, Box::new(value)))
              } else {
                Err(Error::BuildAst)
              }
            }
            "until" => {
              let pred: Ast = arr.iter().nth(1).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              let expr: Ast = arr.iter().nth(2).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              Ok(Ast::Until(Box::new(pred), Box::new(expr)))
            }
            "print" => {
              let expr: Ast = arr.iter().nth(1).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              Ok(Ast::Print(Box::new(expr)))
            }
            "=" => {
              let lhs: Ast = arr.iter().nth(1).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              let rhs: Ast = arr.iter().nth(2).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              Ok(Ast::Eq(Box::new(lhs), Box::new(rhs)))
            }
            "+" => {
              let lhs: Ast = arr.iter().nth(1).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              let rhs: Ast = arr.iter().nth(2).ok_or(Error::BuildAst).and_then(expr_to_ast)?;
              Ok(Ast::Plus(Box::new(lhs), Box::new(rhs)))
            }
            _ => Err(Error::BuildAst),
          }
        }
        _ => Err(Error::BuildAst),
      }
    }
    _ => Err(Error::BuildAst),
  }
}
