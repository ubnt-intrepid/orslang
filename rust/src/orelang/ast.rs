#[allow(dead_code)]
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

pub trait IntoAst {
  fn into_ast(self) -> Result<Ast, super::Error>;
}
