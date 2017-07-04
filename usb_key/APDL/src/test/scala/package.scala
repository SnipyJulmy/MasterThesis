package apdl

import apdl.parser._

package object Utils {
  def strTyp(typ: TfRetTyp): String = typ match {
    case t: TfTyp => t match {
      case p: TfPrimitivesTyp => p match {
        case n: TfNumericTyp => n match {
          case int: TfIntegralTyp => int match {
            case TfInt => "int"
            case TfLong => "long"
            case TfByte => "byte"
            case TfShort => "short"
            case TfChar => "char"
          }
          case float: TfFloatingPointTyp => float match {
            case TfDouble => "double"
            case TfFloat => "float"
          }
        }
        case TfBoolean => "bool"
      }
      case TfArray(arrayTyp) => s"${strTyp(arrayTyp)}[]"
    }
    case TfVoid => "void"
  }
}
