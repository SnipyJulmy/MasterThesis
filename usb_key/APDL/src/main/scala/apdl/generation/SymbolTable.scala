package apdl.generation

import apdl.ApdlCodeGenerationException
import apdl.parser._

import scala.collection.mutable

class SymbolTable {
  val map: mutable.Map[String, SymbolTableElement] = mutable.Map()

  def add(symbol: String, elt: SymbolTableElement): Unit = {
    map.put(symbol, elt)
  }

  def getOption(symbol: String): Option[SymbolTableElement] = {
    map.get(symbol)
  }

  def get(symbol: String): SymbolTableElement = {
    getOption(symbol) match {
      case Some(value) => value
      case None => throw new ApdlCodeGenerationException(s"Unknow symbol $symbol")
    }
  }

  def getsOption(symbols: List[String]): List[Option[SymbolTableElement]] = {
    symbols.map(s => getOption(s))
  }

  def gets(symbols: List[String]): List[SymbolTableElement] = {
    symbols.map(s => getOption(s).getOrElse(throw new ApdlCodeGenerationException(s"Unknow symbol $s")))
  }

  def contains(symbol: String): Boolean = map.contains(symbol)
}

sealed trait SymbolTableElement

case class Component(identifier: String,
                     outputType: ApdlType,
                     parameters: List[Parameter]) extends SymbolTableElement
case class Transform(functionDecl: FunctionDecl) extends SymbolTableElement

sealed trait Input extends SymbolTableElement {
  def getDefinition: ApdlDefine = this match {
    case InputDefault(_, definition, _, _) => definition
    case InputTransformed(_, definition, _) => definition
    case InputComponented(_, definition, _) => definition
  }
}

case class InputDefault(identifier: String,
                        definition: ApdlDefineInput,
                        args: List[String],
                        expr: String) extends Input
case class InputTransformed(identifier: String,
                            definition: ApdlDefineTransform,
                            input: Input) extends Input
case class InputComponented(identifier: String,
                            definition: ApdlDefineComponent,
                            inputs: List[Input]) extends Input