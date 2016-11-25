use super::parse::Expr;

#[derive(Debug)]
pub enum Ast {
  Nil,
  Symbol(String),
  Number(i64),
  Step(Vec<Box<Ast>>),
  Set(String, Box<Ast>),
  Until(Box<Ast>, Box<Ast>),
  Eq(Box<Ast>, Box<Ast>),
  Plus(Box<Ast>, Box<Ast>),
  Neg(Box<Ast>),
  Print(Box<Ast>),
}

pub trait IntoAst {
  fn into_ast(self) -> Result<Ast, super::Error>;
}
