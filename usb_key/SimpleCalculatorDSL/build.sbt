name := "SimpleCalculatorDSL"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-deprecation"
)
