import scala.util.parsing.combinator._
import scala.io.Source

sealed abstract trait Expr
case object Nil extends Expr
case class Bool(b: Boolean) extends Expr
case class Number(n: Int) extends Expr
case class Symbol(s: String) extends Expr
case class Function(name: String, args: Seq[Expr]) extends Expr


object ExprParser extends RegexParsers {
  def ws = """[ \t\n]*""".r

  def _nil: Parser[Expr] =
    "nil".r ~> success(Nil)

  def _boolean: Parser[Expr] =
    "true".r  ~> success(Bool(true)) |
    "false".r ~> success(Bool(false))

  def _number: Parser[Expr] =
    """[\+\-]?[1-9][0-9]+""".r ^^ { case (s) => Number(s.toInt) }

  def _symbol: Parser[Expr] =
    """[a-zA-Z0-9\+\-\*\/\=\!]+""".r ^^ { Symbol(_) }

  def _function: Parser[Expr] =
    "(" ~> ws ~> """[a-zA-Z0-9\+\-\*\/\=\!]+""".r ~ ws ~ repsep(_expr, ws) <~ ")" ^^ {
      case (name ~ ws ~ args) => Function(name, args)
    }

  def _expr: Parser[Expr] =
    ws ~> (_nil | _boolean | _number | _symbol | _function) <~ ws

  def from_str(str: String): Either[String, Expr] =
    parseAll(_expr, str) match {
      case Success(parsed, next) => Right(parsed)
      case NoSuccess(msg, next)  => Left(s"$msg on line ${next.pos.line} on column ${next.pos.column}")
    }

  def from_file(path: String): Either[String, Expr] =
    from_str(Source.fromFile(path).mkString)
}


object Main {
  def main(args: Array[String]) = {
    println(ExprParser.from_str("nil"))
    println(ExprParser.from_str("false"))
    println(ExprParser.from_str("-10"))
    println(ExprParser.from_str("hoge"))
    println(ExprParser.from_str("(+ 1 (+ sum -1))"))
    println(ExprParser.from_file("../examples/example_sum.ore"))
  }
}
