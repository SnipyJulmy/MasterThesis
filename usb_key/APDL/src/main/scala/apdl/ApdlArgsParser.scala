package apdl

import java.io.File

/*
 * TODO TRES IMPORTANT
 * Option pour générer automatiquement l'implémentation du handler
 * On fait un truc crados mais ça va
 */

class ApdlArgsParser {
  def parse(args: Array[String]): ApdlConfig = {

    val argsParser = new scopt.OptionParser[ApdlConfig]("apdl") {
      // Metadata
      head("apdl", "1.0")

      // Argument
      arg[File]("<file>.apdl")
        .action((f, c) => c.copy(mainFile = f))

      opt[File]('d', "dir")
        .action((o, c) => c.copy(outputDirectory = o))
        .text("Output directory")
    }

    argsParser.parse(args, ApdlConfig()) match {
      case Some(value) => value
      case None =>
        println("Unable to parse args")
        System.exit(0)
        throw new Exception("Unreachable code")
    }
  }
}

case class ApdlConfig(mainFile: File = new File("."),
                      outputDirectory: File = new File("./default-apdl-output"),
                      overrideExistingProject: Boolean = true,
                      debug: Boolean = true)
