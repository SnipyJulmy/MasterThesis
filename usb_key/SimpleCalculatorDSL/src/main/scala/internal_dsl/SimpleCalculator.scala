package internal_dsl

trait SimpleCalculator extends App {
  type int = ScInt

  sealed trait ScType {

    def +(that: ScType): ScType = (this, that) match {
      case (ScInt(a), ScInt(b)) => ScInt(a + b)
    }

    def -(that: ScType): ScType = (this, that) match {
      case (ScInt(a), ScInt(b)) => ScInt(a + b)
    }

    def *(that: ScType): ScType = (this, that) match {
      case (ScInt(a), ScInt(b)) => ScInt(a + b)
    }

    def /(that: ScType): ScType = (this, that) match {
      case (ScInt(a), ScInt(b)) => ScInt(a + b)
    }

    override def toString: String = this match {
      case ScInt(value) => s"$value"
    }
  }
  case class ScInt(value: Int) extends ScType

  implicit def Int2ScInt(int: Int) : ScInt = ScInt(int)
}
