package apdl.parser

import scala.io.Source
import scala.util.matching.Regex

/*
 *  Basically, all we are doing here is to replace the @include <file> by the content of the <file> itself...
 */
class IncludeProcessor {
  val include : Regex = "@include \"(.*\\.apdl)\"".r
  def process(code: String): String = {
    include.replaceAllIn(code,rm => s"\n${Source.fromFile(rm.group(1)).mkString}\n")
  }
}
