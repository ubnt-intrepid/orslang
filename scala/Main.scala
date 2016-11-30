import scala.util.parsing.combinator._
import scala.io.Source

sealed abstract trait Expr
case class Bool(b: Boolean) extends Expr
case class Number(n: Int) extends Expr
case class Symbol(s: String) extends Expr
case class List(lst: Seq[Expr]) extends Expr


object ExprParser extends RegexParsers {
  override val skipWhitespace = false

  def ws = """[ \t\n]*""".r
  def ws1 = """[ \t\n]+""".r

  def _boolean: Parser[Expr] =
    "true".r  ~> success(Bool(true)) |
    "false".r ~> success(Bool(false))

  def _number: Parser[Expr] =
    """[\+\-]?[0-9]+""".r ^^ { case (s) => Number(s.toInt) }

  def _symbol: Parser[Expr] =
    """[a-zA-Z\+\-\*\/=!_][a-zA-Z0-9\+\-\*\/=!_]*""".r ^^ { Symbol(_) }

  def _list: Parser[Expr] =
    "(" ~> ws ~> repsep(_expr, ws1) <~ ws <~ ")" ^^ { List(_) }

  def _expr: Parser[Expr] =
    _boolean | _number | _symbol | _list

  def _program: Parser[Expr] =
    ws ~> _expr <~ ws

  def from_str(str: String): Either[String, Expr] =
    parseAll(_program, str) match {
      case Success(parsed, next) => Right(parsed)
      case NoSuccess(msg, next)  => Left(s"$msg on line ${next.pos.line} on column ${next.pos.column}, ${str}")
    }

  def from_file(path: String): Either[String, Expr] =
    from_str(Source.fromFile(path).mkString)
}


object Main {
  def main(args: Array[String]) = {
    println(ExprParser.from_str("false"))
    println(ExprParser.from_str("-10"))
    println(ExprParser.from_str("hoge"))
    println(ExprParser.from_str("(+ 1 (+ sum -1))"))
    println(ExprParser.from_file("../examples/example_sum.ore"))
  }
}
