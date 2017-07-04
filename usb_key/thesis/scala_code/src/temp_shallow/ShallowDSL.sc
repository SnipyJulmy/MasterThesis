sealed trait Temperature {
  def +(that : Temperature) = Add(this,that)
  def in(c : Celsius.type) = InCelsius(this)
}
case class Celsius(temp : Double) extends Temperature
case class Kelvin(temp : Double) extends Temperature
case class Add(t1 : Temperature, t2 : Temperature) extends Temperature
case class InCelsius(temperature: Temperature) extends Temperature

val t1 = Celsius(100)
val t2 = Celsius(100)
val t3 = Kelvin(100)
val t4 = t1 + t2 + t3
println(t4 in Celsius)