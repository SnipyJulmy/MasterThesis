import java.io.{File, PrintWriter, StringWriter}

import apdl.ApdlTestException
import apdl.parser.IncludeProcessor
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class IncludeTest extends FlatSpec with Checkers {

  private val maxFileNameLength = 20
  private val maxNbOfFilesToInclude = 10

  def genApdlFileName: Gen[String] = for {
    l <- Gen.choose(1, maxFileNameLength)
    name <- Gen.listOfN(l, Gen.alphaNumChar)
  } yield name.mkString

  def genIncludableApdlFile: Gen[ApdlIncludableTestFile] = for {
    name <- genApdlFileName
    content <- Gen.listOf(Gen.alphaNumStr) suchThat (_.nonEmpty)
  } yield ApdlIncludableTestFile(name, content.mkString("\n"))

  def genMainApdlFile: Gen[ApdlMainTestFile] = for {
    name <- genApdlFileName
    nbFile <- Gen.choose(0, maxNbOfFilesToInclude)
    fileToIncludes <- Gen.listOfN(nbFile, genIncludableApdlFile)
    content <- Gen.listOf(Gen.alphaNumStr) suchThat (_.nonEmpty)
  } yield ApdlMainTestFile(name, content, fileToIncludes)

  def trimSpace(s: String): String = s
    .replaceAll(" ", "")
    .replaceAll("\n", "")
    .replaceAll("\t", "")
    .replaceAll("\f", "")

  behavior of "IncludeProcessor"

  it should "replace any string inside the double quote by content of that string" in {
    check {
      val includeProcessor = new IncludeProcessor
      forAllNoShrink(genMainApdlFile) { file =>
        // create the main file source
        val main = new StringWriter

        val rndContent = scala.util.Random.shuffle(file.content ::: file.includes)

        val expected = rndContent.map {
          case s: String => s
          case f: ApdlIncludableTestFile => f.content
          case _ => throw new ApdlTestException("Include test failed...")
        }.mkString("\n")

        val content = rndContent.map {
          case s: String => s
          case f: ApdlIncludableTestFile => s"""@include "${f.name}.apdl" """
          case _ => throw new ApdlTestException("Include test failed...")
        }.mkString("\n")

        main.append(content).flush()
        val mainSource = main.toString
        main.close()

        // create all the other file
        val filenames = file.includes.map(_.name)
        file.includes.foreach { f =>
          val name = f.name
          val content = f.content
          val file = new File(s"$name.apdl")
          val pw = new PrintWriter(file)
          pw.append(content).flush()
          pw.close()
        }

        // check property
        val result = includeProcessor.process(mainSource)

        val testResult = trimSpace(result) == trimSpace(expected)

        // delete all file
        filenames.foreach { n =>
          val file = new File(s"$n.apdl")
          file.delete()
        }

        // return result of forAll
        testResult
      }
    }
  }

  case class ApdlIncludableTestFile(name: String,
                                    content: String)

  case class ApdlMainTestFile(name: String,
                              content: List[String],
                              includes: List[ApdlIncludableTestFile])
}