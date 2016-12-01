import scala.collection.mutable.HashMap
import scala.io.Source
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

// base class to represent a engine.
class Engine {
  private val variables = HashMap[String, Expr]()
  private val operators = HashMap[String, (Seq[Expr] => Either[String, Expr])]()

  def evaluate(expr: Expr): Either[String, Expr] =
    expr match {
      case Bool(_) | Number(_) => Right(expr)

      case Symbol(s) =>
        variables.get(s).toRight(s"undefined symbol: `${s}`")

      case List(Symbol(op) +: args) =>
        operators.get(op).toRight(s"undefined operator: `${op}`").map(op => op(args)).joinRight
    }

  def put_variable(sym: String, expr: Expr) =
    variables.put(sym, expr)

  def def_operator (name: String) (op: Seq[Expr] => Either[String, Expr]) =
    operators.put(name, op)
}

class OrelangEngine extends Engine {
  def_operator("step") {
    case expr =>
      expr.map(e => evaluate(e))
          .fold(Right(Nil))((acc, e) => acc.map(_ => e).joinRight)
  }

  def eval_until(pred: Expr, expr: Seq[Expr], res: Expr) : Either[String,Expr] =
    evaluate(pred).right.map({
        case Bool(true) => Right(res)
        case _ =>
          expr.map(e => evaluate(e))
              .fold(Right(Nil))((acc, e) => acc.map(_ => e).joinRight)
              .right.map(res => eval_until(pred, expr, res)).joinRight
      }).joinRight

  def_operator("until") {
    case pred +: expr =>
      eval_until(pred, expr, Nil)

    case _ =>
      Left("invalid arguments")
  }

  def_operator("set") {
    case Symbol(sym) +: e +: _ =>
      evaluate(e).right.map(e => { put_variable(sym, e); Nil })

    case _ => Left("[set] invalid arguments")
  }

  def_operator("=") {
    case e1 +: e2 +: _ => {
      (evaluate(e1), evaluate(e2)) match {
        case (Right(Number(v1)), Right(Number(v2))) => Right(Bool(v1 == v2))
        case _ => Left("[=] cannot substitute")
      }
    }
    case _ => Left("invalid arguments")
  }

  def_operator("+") {
    case e1 +: e2 +: _ => {
      (evaluate(e1), evaluate(e2)) match {
        case (Right(Number(v1)), Right(Number(v2))) => Right(Number(v1 + v2))
        case _ => Left("[+] cannot substitute")
      }
    }
    case _ => Left("invalid arguments")
  }

  def_operator("print") {
    case hd +: _ =>
      println("[print] ", evaluate(hd))
      Right(Nil)
  }
}

object Main extends App {
  // println(ExprParser.from_str("false"))
  // println(ExprParser.from_str("-10"))
  // println(ExprParser.from_str("hoge"))
  // println(ExprParser.from_str("(+ 1 (+ sum -1))"))
  // println(ExprParser.from_file("../examples/example_sum.ore"))

  val eng = new OrelangEngine()
  // println(ExprParser.from_str("42").right.map(eng.evaluate(_)).joinRight)
  // println(ExprParser.from_str("(print 42)").right.map(eng.evaluate(_)).joinRight)
  println(ExprParser.from_file("../examples/example_sum.ore").right.map(eng.evaluate(_)).joinRight)
}
