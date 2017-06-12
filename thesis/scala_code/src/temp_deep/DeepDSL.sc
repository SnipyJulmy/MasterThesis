import scala.language.implicitConversions

implicit def C2K(celsius: Celsius): Kelvin = Kelvin(celsius.temp + 273.15)
implicit def K2C(kelvin: Kelvin): Celsius = Celsius(kelvin.temp - 273.15)

implicit def T2K(temperature: Temperature): Kelvin = temperature match {
  case celsius: Celsius => C2K(celsius)
  case kelvin: Kelvin => kelvin
}

implicit def T2C(temperature: Temperature): Celsius = temperature match {
  case kelvin: Kelvin => K2C(kelvin)
  case celsius: Celsius => celsius
}

sealed abstract class Temperature(val tempInCelsius: Double) {
  def in(c: Celsius.type) = T2C(this)
  def in(k: Kelvin.type) = T2K(this)
  def +(that: Temperature): Temperature
}

case class Celsius(temp: Double) extends Temperature(temp) {
  override def +(that: Temperature): Temperature = add(that)

  private def add(that: Temperature): Celsius = {
    Celsius(T2C(that).temp + temp)
  }
}
case class Kelvin(temp: Double) extends Temperature(temp - 273.15) {
  override def +(that: Temperature): Temperature = add(that)

  private def add(that: Temperature): Kelvin = {
    Kelvin(T2K(that).temp + temp)
  }
}

val t1 = Celsius(100)
val t2 = Celsius(100)
val t3 = Kelvin(100)
val t4 = t1 + t2 + t3
println(t4 in Celsius)