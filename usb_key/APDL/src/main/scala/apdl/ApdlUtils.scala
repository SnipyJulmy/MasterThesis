package apdl

object ApdlUtils {
  def debug(str: String)(implicit config: ApdlConfig): Unit = {
    if (config.debug) {
      println(str)
    }
  }

  def warning(str: String)(implicit config: ApdlConfig): Unit = {
    if (config.debug) {
      println(str)
    }
  }

  def exitOnFailure(): Unit = {
    println(s"EXIT with code 1")
    System.exit(1)
  }

  def exitOnFailure(msg: String)(implicit config: ApdlConfig): Unit = {
    if (config.debug)
      println(msg)
    println(s"EXIT with code 1")
    System.exit(1)
  }

  def exitOnFailure(exception: Throwable)(implicit config: ApdlConfig): Unit = {
    if (config.debug) {
      exception.printStackTrace(System.out)
    }
    println(s"EXIT with code 1")
    System.exit(1)
  }
}
