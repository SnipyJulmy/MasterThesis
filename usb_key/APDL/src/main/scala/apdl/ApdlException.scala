package apdl

abstract class ApdlBaseException(msg: String) extends Throwable {
  def this() = this("")

  override def toString: String = {
    s"$msg ${super.toString}"
  }
}

class ApdlDslException(s: String) extends ApdlBaseException(s)

class ApdlParserException(s: String) extends ApdlBaseException(s)

class ApdlArgsException(s: String) extends ApdlBaseException(s)

class ApdlBackendException(s: String) extends ApdlBaseException(s)

class ApdlFormatException(s: String) extends ApdlParserException(s)

class ApdlTestException(s: String) extends ApdlBaseException(s)

class ApdlProjectException(s: String) extends ApdlBaseException(s)

class ApdlDirectoryException(s: String) extends ApdlBaseException(s)

class ApdlCodeGenerationException(s : String) extends ApdlBaseException(s)