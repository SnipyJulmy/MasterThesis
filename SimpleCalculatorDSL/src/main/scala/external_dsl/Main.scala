package external_dsl

import scala.util.parsing.input.CharSequenceReader

object Main extends App {
  assume(args.length == 1)
  val expr = args(0)

  val parser = new SimpleCalculatorParser
  val ast = parser.parse(
    parser.simpleCalculatorProgram,
    new parser.PackratReader[Char](new CharSequenceReader(expr))
  ).getOrElse(throw new Exception(s"Unable to parse $expr"))

  println(s"$expr = $ast")
  println(s"$expr = ${ast.eval}")
}
