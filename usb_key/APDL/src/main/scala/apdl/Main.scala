package apdl

import apdl.generation.{ApdlProjectManager, ProjectGenerator}

import scala.io.Source

import ApdlUtils._

object Main extends App {

  implicit val config = ApdlConfig()

  val input = "example.apdl"
  val source = Source.fromFile(input).mkString
  val manager = new ApdlProjectManager(source)
  val generator = new ProjectGenerator(manager.project)
  generator.mkProject()
  debug("End")
}
