package apdl.parser

import apdl.ApdlParserException

import scala.Function.tupled
import scala.util.matching.Regex

class MainParsers extends DefineParsers {

  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r

  override def skipWhitespace: Boolean = true

  val ws: Regex = whiteSpace

  def program: Parser[ApdlProject] = {
    def process(xs: List[Object]): ApdlProject = {
      val projectName: String = xs.find(_.isInstanceOf[String]) match {
        case Some(value) => value.asInstanceOf[String]
        case None => throw new ApdlParserException("No project name specifying")
      }

      val devices: List[ApdlDevice] = xs.filter(_.isInstanceOf[ApdlDevice]).map(_.asInstanceOf[ApdlDevice])

      val defineInputs: List[ApdlDefineInput] = xs
        .filter(_.isInstanceOf[ApdlDefineInput])
        .map(_.asInstanceOf[ApdlDefineInput])

      val defineComponents: List[ApdlDefineComponent] = xs
        .filter(_.isInstanceOf[ApdlDefineComponent])
        .map(_.asInstanceOf[ApdlDefineComponent])

      val defineTransforms: List[ApdlDefineTransform] = xs
        .filter(_.isInstanceOf[ApdlDefineTransform])
        .map(_.asInstanceOf[ApdlDefineTransform])

      ApdlProject(projectName, devices, defineInputs, defineComponents, defineTransforms)
    }

    rep1(projectName | apdlDevice | apdlDefine) ^^ {
      xs =>
        process(xs)
    }
  }

  def projectName: Parser[String] = "project_name" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }

  def keyValue: Parser[(String, String)] = identifier ~ "=" ~ identifier ^^ { case (k ~ _ ~ v) => (k, v) }

  def apdlInput: Parser[ApdlInput] = "@input" ~> identifier ~ identifier ~ apdlParameters ^^ {
    case (name ~ typ ~ params) => ApdlInput(name, typ, params)
  }

  def apdlParameters: Parser[List[String]] = rep(apdlParameter)

  def apdlParameter: Parser[String] = "[^ \t\f\n\r{}@]+".r ^^ { str => str }

  def apdlSerial: Parser[ApdlSerial] = "@serial" ~> identifier ~ apdlSampling ^^ {
    case (ident ~ sampling) => ApdlSerial(ident, sampling)
  }

  def apdlSampling: Parser[ApdlSampling] = apdlSamplingUpdate | apdlSamplingTimer

  def apdlSamplingUpdate: Parser[ApdlSamplingUpdate.type] = "update" ^^ { _ => ApdlSamplingUpdate }

  def apdlSamplingTimer: Parser[ApdlSamplingTimer] = "each" ~> "[0-9]+".r ~ timeUnit ^^ { case (value ~ tu) => ApdlSamplingTimer(value.toInt, tu) }

  def timeUnit: Parser[ApdlTimeUnit] = {
    "ns" ^^ { _ => ApdlTimeUnit.ns } |
      "ms" ^^ { _ => ApdlTimeUnit.ms } |
      "s" ^^ { _ => ApdlTimeUnit.s } |
      "m" ^^ { _ => ApdlTimeUnit.m } |
      "h" ^^ { _ => ApdlTimeUnit.h } |
      "d" ^^ { _ => ApdlTimeUnit.d }
  }

  def apdlDevice: Parser[ApdlDevice] = {

    def process(ident: String, xs: List[Object]): ApdlDevice = {
      val inputs = xs.filter(_.isInstanceOf[ApdlInput]).map(_.asInstanceOf[ApdlInput])
      val serials = xs.filter(_.isInstanceOf[ApdlSerial]).map(_.asInstanceOf[ApdlSerial])
      val keyValues = xs.filter(_.isInstanceOf[(String, String)]).map(_.asInstanceOf[(String, String)])
      val framework = (keyValues find tupled((k, _) => k == "framework")).getOrElse(throw new ApdlParserException(s"No framework specify for $ident"))._2
      val id = (keyValues find tupled((k, _) => k == "id")).getOrElse(throw new ApdlParserException(s"No id specify for $ident"))._2
      val parameters = (keyValues filter tupled((k, _) => k != "id" && k != "framework")).toMap
      ApdlDevice(ident, id, framework, inputs, serials, parameters)
    }

    "@device" ~> identifier ~ (lb ~> rep1(keyValue | apdlInput | apdlSerial) <~ rb) ^^ { case (ident ~ xs) => process(ident, xs) }
  }
}

case class ApdlProject(
                        name: String,
                        devices: List[ApdlDevice],
                        defineInputs: List[ApdlDefineInput],
                        defineComponents: List[ApdlDefineComponent],
                        defineTransforms: List[ApdlDefineTransform]
                      )

case class ApdlDevice(name: String, id: String, framework: String, inputs: List[ApdlInput], serials: List[ApdlSerial], additionalParameters: Map[String, String])

case class ApdlInput(identifier: String, defineInputIdentifier: String, args: List[String])

case class ApdlSerial(inputName: String, sampling: ApdlSampling)

sealed trait ApdlSampling
case object ApdlSamplingUpdate extends ApdlSampling
case class ApdlSamplingTimer(value: Int, timeUnit: ApdlTimeUnit) extends ApdlSampling {

  def ms: Int = timeUnit match {
    case ApdlTimeUnit.ns => value / 1000
    case ApdlTimeUnit.ms => value
    case ApdlTimeUnit.s => value * 1000
    case ApdlTimeUnit.m => value * 1000 * 60
    case ApdlTimeUnit.h => value * 1000 * 60 * 60
    case ApdlTimeUnit.d => value * 1000 * 60 * 60 * 24
  }

  def s:Int = timeUnit match {
    case ApdlTimeUnit.ns => value / 1000000
    case ApdlTimeUnit.ms => value / 1000
    case ApdlTimeUnit.s => value
    case ApdlTimeUnit.m => value * 60
    case ApdlTimeUnit.h => value * 60 * 60
    case ApdlTimeUnit.d => value * 60 * 60 * 24
  }
}

sealed trait ApdlTimeUnit
object ApdlTimeUnit {
  case object ns extends ApdlTimeUnit
  case object ms extends ApdlTimeUnit
  case object s extends ApdlTimeUnit
  case object m extends ApdlTimeUnit
  case object h extends ApdlTimeUnit
  case object d extends ApdlTimeUnit

  def values: Seq[ApdlTimeUnit] = Seq(ns, ms, s, m, h, d)
}
