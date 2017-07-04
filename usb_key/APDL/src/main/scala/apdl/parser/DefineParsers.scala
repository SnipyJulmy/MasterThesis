package apdl.parser

import apdl.ApdlCodeGenerationException
import apdl.parser.ApdlType.{Id, Str}

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class DefineParsers extends TransformDslParser with RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r
  override def skipWhitespace: Boolean = true

  lazy val defines: PackratParser[List[ApdlDefine]] = rep(apdlDefine)
  lazy val apdlDefine: PackratParser[ApdlDefine] = "@define" ~> (defineComponent | defineInput | defineTransform)

  lazy val defineComponent: PackratParser[ApdlDefineComponent] = {
    "component" ~> identifier ~ parameters ~ lb ~ defineComponentBody ~ rb ^^ {
      case (i ~ params ~ _ ~ ((in, out, gs)) ~ _) => ApdlDefineComponent(i, params, in, out, gs)
    }
  }

  lazy val defineComponentBody: PackratParser[(Inputs, Output, Map[String, Gen])] = {
    inputs ~ output ~ gens ^^ { case (i ~ o ~ g) => (i, o, g) }
  }

  lazy val inputs: PackratParser[Inputs] = "@in" ~> parameters ^^ { ps => Inputs(ps) }
  lazy val output: PackratParser[Output] = "@out" ~> apdlType ^^ { typ => Output(typ) }
  lazy val gens: PackratParser[Map[String, Gen]] = rep(gen) ^^ { gs => gs.toMap }
  lazy val gen: PackratParser[(String, Gen)] = "@gen" ~> identifier ~ (lb ~> genBody) <~ rb ^^ {
    case (i ~ b) => i -> b
  }

  lazy val genBody: PackratParser[Gen] = {
    global ~ setup ~ loop ~ expr ~ (genType?) ^^ { case (g ~ s ~ l ~ e ~ t) => Gen(g, s, l, e, t) }
  }

  lazy val global: PackratParser[String] = "global" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val setup: PackratParser[String] = "setup" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val loop: PackratParser[String] = "loop" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val expr: PackratParser[String] = "expr" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val literalString: PackratParser[String] = """(\\.|[^\\"])*""".r ^^ { str => str }
  lazy val genType : PackratParser[ApdlType] = "type" ~ "=" ~> apdlStdType

  lazy val defineInput: PackratParser[ApdlDefineInput] = "input" ~> identifier ~ parameters ~ (lb ~> gens <~ rb) ^^ {
    case (defId ~ defParams ~ defGens) => ApdlDefineInput(defId, defParams, defGens)
  }

  lazy val apdlType: PackratParser[ApdlType] = str | id | apdlStdType
  lazy val str: PackratParser[ApdlType.Str.type] = "str" ^^^ ApdlType.Str
  lazy val id: PackratParser[ApdlType.Id.type] = "id" ^^^ ApdlType.Id
  lazy val apdlStdType: PackratParser[ApdlType] = {
    "int" ^^^ ApdlType.Int |
      "float" ^^^ ApdlType.Float |
      "double" ^^^ ApdlType.Double |
      "char" ^^^ ApdlType.Char |
      "byte" ^^^ ApdlType.Byte |
      "short" ^^^ ApdlType.Short |
      "bool" ^^^ ApdlType.Bool |
      "long" ^^^ ApdlType.Long
  }

  lazy val parameters: PackratParser[List[Parameter]] = rep(parameter)
  lazy val parameter: PackratParser[Parameter] = identifier ~ (":" ~> apdlType) ^^ { case (i ~ t) => Parameter(i, t) }

  lazy val number: PackratParser[String] = "[-+]?[0-9]+.?[0-9]*".r ^^ { str => str }

  lazy val defineTransform: PackratParser[ApdlDefineTransform] = {
    "transform" ~> tfFunctionDeclaration ^^ { f => ApdlDefineTransform(f) }
  }
}

case class Inputs(parameters: List[Parameter])
case class Output(outputType: ApdlType)
case class Gen(global: String, setup: String, loop: String, expr: String, typ : Option[ApdlType])

sealed trait ApdlDefine {
  def identifier: String = this match {
    case ApdlDefineInput(name, _, _) => name
    case ApdlDefineComponent(name, _, _, _, _) => name
    case ApdlDefineTransform(functionDecl) => functionDecl.header.identifier
  }
}
case class ApdlDefineInput(name: String, parameters: List[Parameter], gens: Map[String, Gen]) extends ApdlDefine
case class ApdlDefineComponent(name: String, parameters: List[Parameter], inputs: Inputs, outputType: Output, gens: Map[String, Gen]) extends ApdlDefine
case class ApdlDefineTransform(functionDecl: FunctionDecl) extends ApdlDefine

case class Parameter(id: String, typ: ApdlType)

sealed trait ApdlType {
  override def toString: String = this match {
    case Str => "str"
    case Id => "id"
    case ApdlType.Int => "int"
    case ApdlType.Float => "float"
    case ApdlType.Double => "double"
    case ApdlType.Long => "long"
    case ApdlType.Byte => "byte"
    case ApdlType.Short => "short"
    case ApdlType.Bool => "bool"
    case ApdlType.Char => "char"
  }
}

object ApdlType {
  // any string : _AS)D SA)D,...
  case object Str extends ApdlType
  // any valid identifier : _id, asAdASDsa, id_ad_ASDS_ad
  case object Id extends ApdlType
  // standard types
  case object Int extends ApdlType
  case object Float extends ApdlType
  case object Long extends ApdlType
  case object Bool extends ApdlType
  case object Double extends ApdlType
  case object Short extends ApdlType
  case object Char extends ApdlType
  case object Byte extends ApdlType

  def values: Seq[ApdlType] = Seq(Str, Id, Int, Float, Long, Bool, Double, Short, Char, Byte)
  def stdValues: Seq[ApdlType] = Seq(Int, Float, Long, Bool, Double, Short, Char, Byte)
}

object DefineUtils {
  implicit class Defines(defines: List[ApdlDefine]) {
    def defineFromString(stringIdentifier: String): ApdlDefine = {
      defines
        .find(_.identifier == stringIdentifier)
        .getOrElse(throw new ApdlCodeGenerationException(s"Unknow definition input : $stringIdentifier"))
    }
  }
}

