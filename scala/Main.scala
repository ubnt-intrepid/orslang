import scala.util.parsing.combinator._
import scala.io.Source

sealed trait Expr
case class Nil() extends Expr
case class Bool(b: Boolean) extends Expr
case class Number(n: Int) extends Expr
case class Symbol(s: String) extends Expr
case class Function(name: String, args: Seq[Expr]) extends Expr


object ExprParser extends RegexParsers {
  def ws = """[ \t\n]*""".r

  def _nil: Parser[Expr] =
    "nil".r ~> success(Nil())

  def _boolean: Parser[Expr] =
    "true".r ~> success(Bool(true)) | "false".r ~> success(Bool(false))

  def _number: Parser[Expr] =
    """[\+\-]?[1-9][0-9]+""".r ^^ { case (s) => Number(s.toInt) }

  def _symbol: Parser[Expr] =
    """[a-zA-Z0-9\+\-\*\/\=\!]+""".r ^^ { Symbol(_) }

  def _function: Parser[Expr] =
    ws ~> "(" ~> ws ~> _symbol <~ ws <~ repsep(_expr, """[ \t\n]+""".r) <~ ")" <~ ws

  def _expr: Parser[Expr] =
    ws ~> (_nil | _boolean | _number | _symbol | _function) <~ ws

  def apply(str: String): Either[String, Expr] =
    parseAll(_expr, str) match {
      case Success(p, next)     => Right(p)
      case NoSuccess(msg, next) => Left(s"$msg on line ${next.pos.line} on column ${next.pos.column}")
    }
}

object Main {
  def main(args: Array[String]) = {
    println(ExprParser("nil"))
    println(ExprParser("false"))
    println(ExprParser("-10"))
    println(ExprParser("hoge"))
    println(ExprParser("(+ 1 (+ sum -1))"))
    println(ExprParser(Source.fromFile("../examples/example_sum.ore").mkString))
  }
}
