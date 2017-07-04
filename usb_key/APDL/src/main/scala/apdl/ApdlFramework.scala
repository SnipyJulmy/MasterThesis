package apdl

sealed abstract class ApdlFramework(val identifier: String)

object ApdlFramework {
  case object Arduino extends ApdlFramework("arduino")
  case object Mbed extends ApdlFramework("mbed")

  def values: Seq[ApdlFramework] = Seq(Arduino, Mbed)

  def valueOf(framework: String): Option[ApdlFramework] = values.find(f => f.identifier == framework)
}
