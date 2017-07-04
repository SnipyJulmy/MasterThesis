package apdl.generation

import apdl.ApdlParserException
import apdl.parser.{ApdlProject, IncludeProcessor, MainParsers}

import scala.util.parsing.input.CharSequenceReader

class ApdlProjectManager(val source: String) {

  /* Parsers */
  private val mainParsers: MainParsers = new MainParsers

  import mainParsers._

  private val includeProcessor = new IncludeProcessor
  private val processSource = includeProcessor.process(source)
  def project: ApdlProject = mainParsers.parse(mainParsers.program, new PackratReader[Char](new CharSequenceReader(processSource))) match {
    case Success(result, _) =>
      result
    case n: NoSuccess =>
      throw new ApdlParserException(s"$n")
  }
}
