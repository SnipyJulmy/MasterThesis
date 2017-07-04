import apdl.{ApdlParserException, Utils}
import apdl.parser._
import org.scalatest.FlatSpec

import scala.annotation.tailrec
import scala.util.parsing.input.CharSequenceReader
import scala.util.{Random, Try}

class TfTypTest extends FlatSpec {
  val parser = new TransformDslParser {}

  import parser._

  def parseTyp(code: String): TfRetTyp = {
    parser.parse(tfRetType, new PackratReader(new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (next.atEnd) result
        else throw new ApdlParserException(s"Something is not parse : ${next.source}")
      case n: NoSuccess => throw new ApdlParserException(s"Unable to parser $code : $n")
    }
  }

  @tailrec
  final def rndRecArray(limit: Int): Unit = {
    val maxRecArray = 100
    val r = new Random

    def randomTyp(): TfTyp = r.nextInt(8) match {
      case 0 => TfBoolean
      case 1 => TfInt
      case 2 => TfLong
      case 3 => TfByte
      case 4 => TfShort
      case 5 => TfChar
      case 6 => TfDouble
      case 7 => TfFloat
    }

    def newRecArray(tfTyp: TfTyp, depth: Int): TfTyp = depth match {
      case 0 => tfTyp
      case n => TfArray(newRecArray(tfTyp, n - 1))
    }

    def inner(): Unit = {
      val depth = r.nextInt(maxRecArray) + 1 // avoid 0
      val typ = randomTyp()
      val code = s"${Utils.strTyp(typ)}${"[]" * depth}"
      val ast = parser.parse(tfRetType, new PackratReader(new CharSequenceReader(code))) match {
        case Success(result, _) => result
        case _: NoSuccess => fail()
      }
      assert(ast == newRecArray(typ, depth))
    }

    limit match {
      case 0 =>
        inner()
      case n =>
        inner()
        rndRecArray(n - 1)
    }
  }

  s"int" should s"be convert to ${
    TfInt
  }" in assert(parseTyp("int") == TfInt)
  s"bool" should s"be convert to ${
    TfBoolean
  }" in assert(parseTyp("bool") == TfBoolean)
  s"float" should s"be convert to ${
    TfFloat
  }" in assert(parseTyp("float") == TfFloat)
  s"double" should s"be convert to ${
    TfDouble
  }" in assert(parseTyp("double") == TfDouble)
  s"long" should s"be convert to ${
    TfLong
  }" in assert(parseTyp("long") == TfLong)
  s"byte" should s"be convert to ${
    TfByte
  }" in assert(parseTyp("byte") == TfByte)
  s"char" should s"be convert to ${
    TfChar
  }" in assert(parseTyp("char") == TfChar)
  s"short" should s"be convert to ${
    TfShort
  }" in assert(parseTyp("short") == TfShort)
  s"void" should s"be convert to ${
    TfVoid
  }" in assert(parseTyp("void") == TfVoid)

  s"int[]" should s"be convert to ${
    TfArray(TfInt)
  }" in assert(parseTyp("int[]") == TfArray(TfInt))
  s"float[]" should s"be convert to ${
    TfArray(TfFloat)
  }" in assert(parseTyp("float[]") == TfArray(TfFloat))
  s"char[]" should s"be convert to ${
    TfArray(TfChar)
  }" in assert(parseTyp("char[]") == TfArray(TfChar))
  s"byte[]" should s"be convert to ${
    TfArray(TfByte)
  }" in assert(parseTyp("byte[]") == TfArray(TfByte))
  s"double[]" should s"be convert to ${
    TfArray(TfDouble)
  }" in assert(parseTyp("double[]") == TfArray(TfDouble))
  s"long[]" should s"be convert to ${
    TfArray(TfLong)
  }" in assert(parseTyp("long[]") == TfArray(TfLong))
  s"bool[]" should s"be convert to ${
    TfArray(TfBoolean)
  }" in assert(parseTyp("bool[]") == TfArray(TfBoolean))
  s"short[]" should s"be convert to ${
    TfArray(TfShort)
  }" in assert(parseTyp("short[]") == TfArray(TfShort))

  s"int[][]" should s"be convert to ${
    TfArray(TfArray(TfInt))
  }" in assert(parseTyp("int[][]") == TfArray(TfArray(TfInt)))
  s"float[][]" should s"be convert to ${
    TfArray(TfArray(TfFloat))
  }" in assert(parseTyp("float[][]") == TfArray(TfArray(TfFloat)))
  s"char[][]" should s"be convert to ${
    TfArray(TfArray(TfChar))
  }" in assert(parseTyp("char[][]") == TfArray(TfArray(TfChar)))
  s"byte[][]" should s"be convert to ${
    TfArray(TfArray(TfByte))
  }" in assert(parseTyp("byte[][]") == TfArray(TfArray(TfByte)))
  s"double[][]" should s"be convert to ${
    TfArray(TfArray(TfDouble))
  }" in assert(parseTyp("double[][]") == TfArray(TfArray(TfDouble)))
  s"long[][]" should s"be convert to ${
    TfArray(TfArray(TfLong))
  }" in assert(parseTyp("long[][]") == TfArray(TfArray(TfLong)))
  s"bool[][]" should s"be convert to ${
    TfArray(TfArray(TfBoolean))
  }" in assert(parseTyp("bool[][]") == TfArray(TfArray(TfBoolean)))
  s"short[][]" should s"be convert to ${
    TfArray(TfArray(TfShort))
  }" in assert(parseTyp("short[][]") == TfArray(TfArray(TfShort)))

  s"void[]" should "not pass" in assert {
    Try(parseTyp("void[]")) match {
      case util.Failure(_) => true
      case util.Success(_) => false
    }
  }

  s"void[][]" should "not pass" in assert {
    Try(parseTyp("void[][]")) match {
      case util.Failure(_) => true
      case util.Success(_) => false
    }
  }

  "Some random generated array" should "produce the correct ast" in {
    rndRecArray(1000)
  }
}
