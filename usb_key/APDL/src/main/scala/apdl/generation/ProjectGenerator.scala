package apdl.generation

import java.io.{File, PrintWriter}

import apdl.ApdlUtils._
import apdl._
import apdl.parser.{ApdlDevice, ApdlProject}

import scala.util.{Failure, Try}

/**
  * Class which provides the generation of a project
  * A project is the following :
  * A root directory
  * A directory per device declared in the apdl file :
  *   - each of those directory is a valid platformio project with :
  *     - a platformio.ini file as project descriptor
  *     - a src file, with the generated code
  *     - a lib file, in which there is some library needed
  */
class ProjectGenerator(project: ApdlProject)(implicit config: ApdlConfig) {

  import PlatformIOBoardJsonProtocol._

  private val rootOutputDir = config.outputDirectory
  implicit private val debugEnable = config.debug


  def mkProject(): Unit = {
    Try {
      // Create the root directory
      mkDir(rootOutputDir)

      // Create the device project directory
      val deviceProjects = project.devices.map { device =>
        val name = device.name
        val root = mkDir(rootOutputDir.getAbsolutePath + "/" + name)
        val src = mkDir(rootOutputDir.getAbsolutePath + "/" + name + "/src")
        val lib = mkDir(rootOutputDir.getAbsolutePath + "/" + name + "/lib")
        DeviceProject(root, src, lib, project, device)
      }

      // Generate the project for each device
      deviceProjects.foreach(_.generateProject())
    } match {
      case Failure(exception) => exitOnFailure(exception)
      case _ =>
    }
  }

  def recursiveDelete(file: File): Boolean = Option(file) match {
    case Some(root) =>
      Option(root.listFiles()) match {
        case Some(files) =>
          files.forall(recursiveDelete)
          debug(s"remove ${root.getAbsolutePath}")
          root.delete()
        case None =>
          debug(s"remove ${root.getAbsolutePath}")
          root.delete()
      }
    case None => true
  }

  def mkDir(dir: File): File = {
    if (dir.exists()) {
      if (config.overrideExistingProject) {
        if (recursiveDelete(dir)) {
          debug(s"${dir.getPath} has been deleted")
        }
        else {
          throw new ApdlDirectoryException(s"Can't remove ${dir.getPath}")
        }
      }
      else {
        throw new ApdlDirectoryException(s"${dir.getPath} already exist, use the override option or remove the directory")
      }
    }
    else {
      debug(s"${dir.getPath} don't exist, nothing to do")
    }

    if (dir.mkdir())
      debug(s"${dir.getPath} has been created")
    else {
      throw new ApdlDirectoryException(s"${dir.getPath} can't be create")
    }
    dir
  }

  def mkDir(dirName: String): File = {
    val dir = new File(dirName)
    mkDir(dir)
  }


  case class PlatformIOIniInfo(boardsId: String, framework: String, platform: String, libForce: Option[String], libDeps: List[String]) {
    def mkFile(rootDir: File): Unit = {
      if (rootDir.exists())
        if (rootDir.isDirectory) {
          val outputFile = new File(rootDir.getAbsolutePath, "platformio.ini")
          if (!outputFile.createNewFile())
            throw new ApdlProjectException(s"Can't create file ${outputFile.getAbsolutePath}")
          val outputStream = new PrintWriter(outputFile)
          outputStream.append(s"[env:$boardsId]\n")
          outputStream.append(s"platform = $platform\n")
          outputStream.append(s"board = $boardsId\n")
          outputStream.append(s"framework = $framework\n")
          if (libDeps.nonEmpty) {
            outputStream.append(
              s"""
                 |lib_deps =
                 |  ${libDeps.map(l => s"\t$l") mkString "\n"}
               """.stripMargin)
          }
          libForce match {
            case Some(value) => outputStream.append(s"lib_force = $value")
            case _ =>
          }
          outputStream.flush()
          outputStream.close()
        }
        else throw new ApdlProjectException(s"${rootDir.getAbsolutePath} is not a directory")
      else throw new ApdlProjectException(s"${rootDir.getAbsolutePath} did not exist")
    }
  }

  def generatePlatformIOIni(targetDir: File, info: PlatformIOIniInfo): Unit = {
    if (!targetDir.exists())
      throw new ApdlDirectoryException(s"$targetDir don't exist")
    if (!targetDir.isDirectory)
      throw new ApdlDirectoryException(s"$targetDir isn't a directory")
    info.mkFile(targetDir)
  }

  case class DeviceProject(rootDir: File, srcDir: File, libDir: File, project: ApdlProject, device: ApdlDevice) {
    def generateProject(): Unit = {
      // Generate the platformio project
      val boards = getBoards(device.id)
      val board = boards.find(_.id == device.id).getOrElse(throw new ApdlProjectException(s"Can't get info for board : ${device.id}"))
      val platform = board.platform
      val platformIOIniInfo: PlatformIOIniInfo = PlatformIOIniInfo(device.id, device.framework, platform, None, List("Timer"))
      generatePlatformIOIni(rootDir, platformIOIniInfo)

      // generate the src
      val generator = new CLikeCodeGenerator(project, device)
      generator.mkDevice(srcDir)
    }
  }
}



