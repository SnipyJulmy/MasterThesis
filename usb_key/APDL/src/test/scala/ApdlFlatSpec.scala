import apdl.ApdlParserException
import apdl.parser.MainParsers
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.util.parsing.input.CharSequenceReader

abstract class ApdlFlatSpec extends FlatSpec with Checkers {

  val parser = new MainParsers

  import parser._

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSize = 500,
    sizeRange = 100
  )

  protected def parse[A](code: String, astParser: Parser[A]): A = {
    parser.parse(astParser, new PackratReader[Char](new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (!dropWs(next).atEnd) throw new ApdlParserException(s"Unable to parse completely $code: $next")
        else result
      case n: NoSuccess =>
        if (code != "") throw new ApdlParserException(s"Unable to parse $code: $n")
        else throw new ApdlParserException(s"Unable to parse '': $n")
    }
  }

  private def dropWs(input: parser.Input): parser.Input = {
    if (input.atEnd)
      input
    else {
      if (parser.ws.pattern.matcher(input.first.toString).matches())
        dropWs(input.rest)
      else
        input
    }
  }
}
