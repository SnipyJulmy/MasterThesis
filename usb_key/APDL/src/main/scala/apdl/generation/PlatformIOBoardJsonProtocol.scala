package apdl.generation

import spray.json._

import scala.sys.process._

/**
  * Serialization from platformio json output to PlatformIOBoard case class
  */
object PlatformIOBoardJsonProtocol extends DefaultJsonProtocol {

  implicit val boardFormat: RootJsonFormat[PlatformIOBoard] = jsonFormat10(PlatformIOBoard)

  def getBoards(boardId: String): Vector[PlatformIOBoard] = {
    val json = s"platformio boards --json-output $boardId" !!
    val ast = json.parseJson
    val boards = ast.asInstanceOf[JsArray].elements.map(_.convertTo[PlatformIOBoard])
    boards
  }
}

case class PlatformIOBoard(platform: String, url: String, vendor: String, name: String, frameworks: List[String],
                           fcpu: Int, mcu: String, ram: Int, id: String, rom: Int)