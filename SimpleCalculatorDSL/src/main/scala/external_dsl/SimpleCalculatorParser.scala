package external_dsl

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class SimpleCalculatorParser extends RegexParsers with PackratParsers {
  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r

  override def skipWhitespace: Boolean = true

  lazy val simpleCalculatorProgram: PackratParser[Expr] = expr

  lazy val expr: PackratParser[Expr] = term

  lazy val term: PackratParser[Expr] = {
    term ~ "+" ~ product ^^ { case (l ~ _ ~ r) => Add(l, r) } |
      term ~ "-" ~ product ^^ { case (l ~ _ ~ r) => Sub(l, r) } |
      product
  }

  lazy val product: PackratParser[Expr] = {
    product ~ "*" ~ operand ^^ { case (l ~ _ ~ r) => Mul(l, r) } |
      product ~ "/" ~ operand ^^ { case (l ~ _ ~ r) => Div(l, r) } |
      operand
  }

  lazy val operand: PackratParser[Expr] = number | "(" ~> expr <~ ")"
  lazy val number: PackratParser[Expr] = "[0-9]+".r ^^ { str => Number(str.toInt) }
}

sealed trait Expr {
  def eval: Int = this match {
    case Number(value) => value
    case Add(left, right) => left.eval + right.eval
    case Mul(left, right) => left.eval * right.eval
    case Sub(left, right) => left.eval - right.eval
    case Div(left, right) => left.eval / right.eval
  }
}
case class Number(value: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
