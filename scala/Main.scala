import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.control.Breaks
import scala.util.parsing.combinator._

sealed abstract trait Expr
case object Nil extends Expr
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

class Engine {
  var variables = HashMap[String, Expr]()
  var operators = HashMap[String, (Seq[Expr] => Either[String, Expr])]()

  operators.put("step", expr => {
    expr.map(e => evaluate(e))
        .fold(Right(Nil))((acc, e) => acc match {
            case Right(_) => e
            case Left(err) => Left(err)
          })
  })

  operators.put("until", expr => {
    expr match {
      case pred +: e +: _ => {
        var res: Either[String,Expr] = Right(Nil)
        val b = new Breaks
        b.breakable {
          while (true) {
            evaluate(pred) match {
              case Right(Bool(true)) => b.break
              case Right(_) => {
                evaluate(e) match {
                  case Right(_) => ()
                  case Left(e) => { res = Left(e); b.break }
                }
              }
              case Left(e) => { res = Left(e); b.break }
            }
          }
        }
        res
      }
      case _ => Left("invalid arguments")
    }
  })

  operators.put("set", expr => {
    expr match {
      case Symbol(sym) +: e +: _ => {
        evaluate(e) match {
          case Right(e) => { variables.put(sym, e); Right(Nil) }
          case Left(err) => Left(err)
        }
      }
      case _ => Left("[set] invalid arguments")
    }
  })

  operators.put("=", expr => {
    expr match {
      case e1 +: e2 +: _ => {
        (evaluate(e1), evaluate(e2)) match {
          case (Right(Number(v1)), Right(Number(v2))) => Right(Bool(v1 == v2))
          case _ => Left("[=] cannot substitute")
        }
      }
      case _ => Left("invalid arguments")
    }
  })

  operators.put("+", expr => {
    expr match {
      case e1 +: e2 +: _ => {
        (evaluate(e1), evaluate(e2)) match {
          case (Right(Number(v1)), Right(Number(v2))) => Right(Number(v1 + v2))
          case _ => Left("[+] cannot substitute")
        }
      }
      case _ => Left("invalid arguments")
    }
  })

  operators.put("print", expr => {
    println("[print] ", evaluate(expr.head))
    Right(expr.head)
  })

  def evaluate(expr: Expr): Either[String, Expr] = {
    expr match {
      case Bool(_) | Number(_) => Right(expr)
      case Symbol(s) => variables.get(s) match {
        case Some(e) => Right(e)
        case None => Left(s"undefined symbol: `${s}`")
      }
      case List(Symbol(op) +: args) => operators.get(op) match {
        case Some(op) => op(args)
        case None => Left(s"undefined operator: `${op}`")
      }
    }
  }
}

object Main {
  def main(args: Array[String]) = {
    // println(ExprParser.from_str("false"))
    // println(ExprParser.from_str("-10"))
    // println(ExprParser.from_str("hoge"))
    // println(ExprParser.from_str("(+ 1 (+ sum -1))"))
    // println(ExprParser.from_file("../examples/example_sum.ore"))

    val eng = new Engine()
    // println(ExprParser.from_str("42").right.map(eng.evaluate(_)).joinRight)
    // println(ExprParser.from_str("(print 42)").right.map(eng.evaluate(_)).joinRight)
    println(ExprParser.from_file("../examples/example_sum.ore").right.map(eng.evaluate(_)).joinRight)
  }
}
