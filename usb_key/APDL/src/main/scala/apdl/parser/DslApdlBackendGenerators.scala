package apdl.parser

import apdl.parser.ApdlTimeUnit._
import apdl.parser.ApdlType.{Bool, Byte, Char, Double, Float, Id, Int, Long, Short, Str}

import scala.Function.tupled

/**
  * An code generators which target the apdl language itself
  * Primarly use for test and try
  */
trait DslApdlBackendGenerators extends TransformApdlBackendGenerators {

  def toApdlCode(define: ApdlDefine): String = define match {
    case ApdlDefineInput(name, parameters, gens) =>
      s"""
         |@define input $name ${parameters map toApdlCode mkString " "} {
         |  ${gens map toApdlCode mkString "\n"}
         |}
       """.stripMargin
    case ApdlDefineComponent(name, parameters, inputs, output, gens) =>
      s"""
         |@define component $name ${parameters map toApdlCode mkString " "} {
         |  ${toApdlCode(inputs)}
         |  ${toApdlCode(output)}
         |  ${gens map toApdlCode mkString "\n"}
         |}
       """.stripMargin
    case ApdlDefineTransform(functionDecl) =>
      s"""
         |@define transform ${toApdlCode(functionDecl)}
       """.stripMargin
  }

  def toApdlCode(parameter: Parameter): String = s"${parameter.id} : ${parameter.typ}"

  def toApdlCode(gen: (String, apdl.parser.Gen)): String = {
    val (id, g) = gen
    s"""
       |@gen $id {
       |  ${toApdlCode(g)}
       |}
     """.stripMargin
  }

  def toApdlCode(inputs: Inputs): String = s"@in ${toApdlCode(inputs.parameters)}"

  def toApdlCode(output: Output): String = s"@out ${toApdlCode(output.outputType)}"

  def toApdlCode(outputType: ApdlType): String = outputType match {
    case Str => "str"
    case Id => "id"
    case Int => "int"
    case Float => "float"
    case Long => "long"
    case Bool => "bool"
    case Double => "double"
    case Short => "short"
    case Char => "char"
    case Byte => "byte"
  }

  def toApdlCode(gen: Gen): String =
    s"""
       |global = "${gen.global}"
       |setup = "${gen.setup}"
       |loop = "${gen.loop}"
       |expr = "${gen.expr}"
       |${gen.typ match {
      case Some(value) => s"type = ${toApdlCode(value)}"
      case None => ""
    }}
     """.stripMargin

  def toApdlCode(x: Map[String, Gen]): String = x.map { case (k, v) =>
    s"""
       |@gen $k {
       | ${toApdlCode(v)}
       |}
    """.stripMargin
  } mkString "\n"

  def toApdlCode(parameters: Seq[Parameter]): String = parameters.map(toApdlCode).mkString(" ")

  def toApdlCode(device: ApdlDevice): String =
    s"""
       |@device ${device.name} {
       |  id = ${device.id}
       |  framework = ${device.framework}
       |  ${device.inputs map toApdlCode mkString "\n"}
       |  ${device.serials map toApdlCode mkString "\n"}
       |  ${device.additionalParameters map tupled((k, v) => s"$k = $v") mkString "\n"}
       |}
     """.stripMargin

  def toApdlCode(input: ApdlInput): String =
    s"@input ${input.identifier} ${input.defineInputIdentifier} ${input.args mkString " "}"

  def toApdlCode(serial: ApdlSerial): String = s"@serial ${serial.inputName} ${toApdlCode(serial.sampling)}"

  def toApdlCode(timeUnit: ApdlTimeUnit): String = timeUnit match {
    case _: ns.type => "ns"
    case _: ms.type => "ms"
    case _: s.type => "s"
    case _: m.type => "m"
    case _: h.type => "h"
    case _: d.type => "d"
  }

  def toApdlCode(sampling: ApdlSampling): String = sampling match {
    case ApdlSamplingUpdate => "update"
    case ApdlSamplingTimer(value, timeUnit) => s"each $value ${toApdlCode(timeUnit)}"
  }

  def toApdlCode(project: ApdlProject): String =
    s"""
       |project_name = "${project.name}"
       |
       |${project.devices map toApdlCode mkString "\n"}
       |
       |${project.defineComponents map toApdlCode mkString "\n"}
       |
       |${project.defineInputs map toApdlCode mkString "\n"}
       |
       |${project.defineTransforms map toApdlCode mkString "\n"}
     """.stripMargin
}
