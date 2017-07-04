package apdl

import java.util.concurrent.TimeUnit

import org.influxdb.dto.Point

import scala.language.implicitConversions

/**
  * Created by snipy
  * Project visualization-server
  */
package object Utils {
  implicit def Str2ColorStr(string: String): ColorStr = new ColorStr(string)

  class ColorStr(string: String) {

    import Console._

    // Text color
    def black: String = BLACK + string + RESET

    def red: String = RED + string + RESET

    def blue: String = BLUE + string + RESET

    def yellow: String = YELLOW + string + RESET

    def magenta: String = MAGENTA + string + RESET

    def cyan: String = CYAN + string + RESET

    def white: String = WHITE + string + RESET

    def green: String = GREEN + string + RESET

    // background color
    def onBlack: String = BLACK_B + string + RESET

    def onRed: String = RED_B + string + RESET

    def onBlue: String = BLUE_B + string + RESET

    def onYellow: String = YELLOW_B + string + RESET

    def onMagenta: String = MAGENTA_B + string + RESET

    def onCyan: String = CYAN_B + string + RESET

    def onWhite: String = WHITE_B + string + RESET

    def onGreen: String = GREEN_B + string + RESET

    // text properties
    def bold: String = BOLD + string + RESET

    def underline: String = UNDERLINED + string + RESET

    def blink: String = BLINK + string + RESET

    def reverse: String = REVERSED + string + RESET

    def invisible: String = INVISIBLE + string + RESET
  }

  case class InfluxDBField(name: String, value: InfluxDBType)

  sealed trait InfluxDBType {
    def value(): AnyVal = this match {
      case InfluxDBBoolean(b) => b
      case InfluxDBInt(i) => i
      case InfluxDBDouble(d) => d
      case InfluxDBLong(l) => l
    }
  }

  case class InfluxDBBoolean(b: Boolean) extends InfluxDBType

  case class InfluxDBInt(i: Int) extends InfluxDBType

  case class InfluxDBDouble(d: Double) extends InfluxDBType

  case class InfluxDBLong(l: Long) extends InfluxDBType

  implicit def Boolean2InfluxDBBoolean(boolean: Boolean): InfluxDBBoolean = InfluxDBBoolean(boolean)

  implicit def Double2InfluxDBDouble(double: Double): InfluxDBDouble = InfluxDBDouble(double)

  implicit def Int2InfluxDBInt(int: Int): InfluxDBInt = InfluxDBInt(int)

  implicit def Long2InfluxDBLong(long: Long): InfluxDBLong = InfluxDBLong(long)
  def PointBuilder(measurement: String,
                   time: Long,
                   timeUnit: TimeUnit,
                   influxDBFields: InfluxDBField*): Point = {
    def addField(p: Point.Builder, field: InfluxDBField): Point.Builder =
      field.value match {
        case InfluxDBBoolean(value) => p.addField(field.name, value)
        case InfluxDBInt(value) => p.addField(field.name, value)
        case InfluxDBDouble(value) => p.addField(field.name, value)
        case InfluxDBLong(value) => p.addField(field.name, value)
      }

    def applyField(p: Point.Builder, fields: Seq[InfluxDBField]): Point = {
      fields match {
        case Seq(f) => addField(p, f).build()
        case Seq(f, xs@_*) => applyField(addField(p, f), xs)
      }
    }

    applyField(Point
      .measurement(measurement)
      .time(time, timeUnit), influxDBFields)
  }
}
