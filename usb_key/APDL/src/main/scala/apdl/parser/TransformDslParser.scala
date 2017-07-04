package apdl.parser

import apdl._

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

/* Transformater script syntax */
trait TransformDslParser extends RegexParsers with PackratParsers {
  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r
  override def skipWhitespace: Boolean = true

  // Types
  lazy val tfRetType: PackratParser[TfRetTyp] = tfVoid | tfTyp
  lazy val tfTyp: PackratParser[TfTyp] = tfArrayTyp | tfPrimitivesTyp
  lazy val tfPrimitivesTyp: PackratParser[TfPrimitivesTyp] = tfBooleanTyp | tfNumericTyp
  lazy val tfBooleanTyp: PackratParser[TfBoolean.type] = "bool" ^^ { _ => TfBoolean }
  lazy val tfNumericTyp: PackratParser[TfNumericTyp] = tfIntegralTyp | tfFloatingPointTyp
  lazy val tfIntegralTyp: PackratParser[TfIntegralTyp] = tfInt | tfShort | tfLong | tfByte | tfChar
  lazy val tfInt: PackratParser[TfInt.type] = "int" ^^ { _ => TfInt }
  lazy val tfLong: PackratParser[TfLong.type] = "long" ^^ { _ => TfLong }
  lazy val tfByte: PackratParser[TfByte.type] = "byte" ^^ { _ => TfByte }
  lazy val tfShort: PackratParser[TfShort.type] = "short" ^^ { _ => TfShort }
  lazy val tfChar: PackratParser[TfChar.type] = "char" ^^ { _ => TfChar }
  lazy val tfFloatingPointTyp: PackratParser[TfFloatingPointTyp] = tfFloat | tfDouble
  lazy val tfFloat: PackratParser[TfFloat.type] = "float" ^^ { _ => TfFloat }
  lazy val tfDouble: PackratParser[TfDouble.type] = "double" ^^ { _ => TfDouble }
  lazy val tfArrayTyp: PackratParser[TfArray] = tfTyp <~ "[" ~ "]" ^^ { typ => TfArray(typ) }
  lazy val tfVoid: PackratParser[TfVoid.type] = "void" ^^ { _ => TfVoid }

  // Expressions

  lazy val tfConstantExpr: PackratParser[Expr] = {
    tfLogicalOrExpr
  }

  lazy val tfLogicalOrExpr: PackratParser[Expr] = {
    tfLogicalOrExpr ~ ("||" ~> tfLogicalAndExpr) ^^ { case (l ~ r) => Or(l, r) } |
      tfLogicalAndExpr
  }

  lazy val tfLogicalAndExpr: PackratParser[Expr] = {
    tfLogicalAndExpr ~ ("&&" ~> tfEqualityExpr) ^^ { case (l ~ r) => And(l, r) } |
      tfEqualityExpr
  }

  lazy val tfEqualityExpr: PackratParser[Expr] = {
    tfEqualityExpr ~ ("==" ~> tfRelationalExpr) ^^ { case (l ~ r) => Equals(l, r) } |
      tfEqualityExpr ~ ("!=" ~> tfRelationalExpr) ^^ { case (l ~ r) => NotEquals(l, r) } |
      tfRelationalExpr
  }

  lazy val tfRelationalExpr: PackratParser[Expr] = {
    tfRelationalExpr ~ (">" ~> tfAdditiveExpr) ^^ { case (l ~ r) => Greater(l, r) } |
      tfRelationalExpr ~ ("<" ~> tfAdditiveExpr) ^^ { case (l ~ r) => Smaller(l, r) } |
      tfRelationalExpr ~ (">=" ~> tfAdditiveExpr) ^^ { case (l ~ r) => GreaterEquals(l, r) } |
      tfRelationalExpr ~ ("<=" ~> tfAdditiveExpr) ^^ { case (l ~ r) => SmallerEquals(l, r) } |
      tfAdditiveExpr
  }

  lazy val tfAdditiveExpr: PackratParser[Expr] = {
    tfAdditiveExpr ~ ("+" ~> tfMultiplicativeExpr) ^^ { case (l ~ r) => Add(l, r) } |
      tfAdditiveExpr ~ ("-" ~> tfMultiplicativeExpr) ^^ { case (l ~ r) => Sub(l, r) } |
      tfMultiplicativeExpr
  }

  lazy val tfMultiplicativeExpr: PackratParser[Expr] = {
    tfMultiplicativeExpr ~ ("*" ~> tfCastExpr) ^^ { case (l ~ r) => Mul(l, r) } |
      tfMultiplicativeExpr ~ ("/" ~> tfCastExpr) ^^ { case (l ~ r) => Div(l, r) } |
      tfCastExpr
  }

  lazy val tfCastExpr: PackratParser[Expr] = {
    (lp ~> tfPrimitivesTyp <~ rp) ~ tfCastExpr ^^ { case (t ~ expr) => Cast(t, expr) } |
      tfNotExpr
  }

  lazy val tfNotExpr: PackratParser[Expr] = {
    "!" ~> tfNotExpr ^^ { e => Not(e) } |
      tfPostfixExpr
  }

  lazy val tfPostfixExpr: PackratParser[Expr] = tfFunctionCall | tfArrayAccess | tfPrimaryExpr

  lazy val tfPrimaryExpr: PackratParser[Expr] = {
    tfAtom | tfSymbol | tfLiteral | lp ~> tfExpr <~ rp
  }

  lazy val tfArrayAccess: PackratParser[Expr] = {
    identifier ~ rep1("[" ~> tfExpr <~ "]") ^^ { case (id ~ expr) =>
      expr.tail.foldLeft(ArrayAccess(Symbol(id), expr.head))((acc, elt) => ArrayAccess(acc, elt))
    }
  }

  lazy val tfAtom: PackratParser[Expr] = {
    "true" ^^ { _ => True() } |
      "false" ^^ { _ => False() }
  }

  lazy val tfExpr: PackratParser[Expr] = {
    tfAssignExpr
  }

  lazy val tfAssignExpr: PackratParser[Expr] = {
    tfPostfixExpr ~ ("=" ~> tfAssignExpr) ^^ { case (l ~ r) => VarAssignement(l, r) } |
      tfLogicalOrExpr
  }

  lazy val tfSymbol: PackratParser[Symbol] = {
    identifier ^^ { x => Symbol(x) }
  }

  lazy val tfLiteral: PackratParser[Literal] = {
    """[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)(E[0-9]+)?""".r ^^ Literal
  }

  lazy val identifier: PackratParser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  lazy val tfFunctionCall: PackratParser[FunctionCall] =
    identifier ~ lp ~ tfFunctionCallArg ~ rp ^^ {
      case (id ~ _ ~ args ~ _) => FunctionCall(id, args)
    }

  lazy val tfFunctionCallArg: PackratParser[List[Expr]] = {
    (tfExpr ?) ~ rep("," ~> tfExpr) ^^ { case (a ~ as) =>
      a match {
        case Some(value) => value :: as
        case None => List()
      }
    }
  }

  lazy val tfArgs: PackratParser[List[TypedIdentifier]] = tfArg ~ rep("," ~> tfArg) ^^ {
    case (a ~ as) => a :: as
  }

  lazy val tfArg: PackratParser[TypedIdentifier] = {
    identifier ~ ":" ~ tfTyp ^^ { case (id ~ _ ~ typ) => TypedIdentifier(id, typ) }
  }

  lazy val tfVarAssign: PackratParser[VarAssignement] = {
    identifier ~ "=" ~ tfConstantExpr ^^ { case (id ~ _ ~ expr) => VarAssignement(Symbol(id), expr) }
  }

  lazy val tfBlock: PackratParser[Block] = lb ~> tfStatements <~ rb ^^ { statements => Block(statements) }

  lazy val tfStatements: PackratParser[List[Statement]] = rep(tfStatement)

  lazy val tfStatement: PackratParser[Statement] = {
    tfBlock | tfSelectionStatement | tfLoop | tfJump | tfDeclaration | tfExprStatement
  }

  lazy val tfExprStatement: PackratParser[ExpressionStatement] = tfExpr ^^ { expr => ExpressionStatement(expr) }

  lazy val tfLoop: PackratParser[Statement] = tfWhile | tfDoWhile
  lazy val tfWhile: PackratParser[While] = "while" ~ lp ~ tfExpr ~ rp ~ tfStatement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => While(cond, statement)
  }
  lazy val tfDoWhile: PackratParser[DoWhile] = "do" ~ tfStatement ~ "while" ~ lp ~ tfExpr ~ rp ^^ {
    case (_ ~ statement ~ _ ~ _ ~ cond ~ _) => DoWhile(cond, statement)
  }

  lazy val tfDeclaration: PackratParser[Declaration] = {
    tfNewVal | tfNewArray | tfNewVar | tfFunctionDeclaration
  }

  lazy val tfFunctionDeclaration: PackratParser[FunctionDecl] = {
    "def" ~> tfFunctionHeader ~ tfFunctionBody ^^ { case (h ~ b) => FunctionDecl(h, b) }
  }

  lazy val tfFunctionHeader: Parser[FunctionHeader] =
    identifier ~ lp ~ tfArgs ~ rp ~ "->" ~ tfRetType ^^ {
      case (id ~ _ ~ parameters ~ _ ~ "->" ~ ret_type) => FunctionHeader(ret_type, id, parameters)
    } |
      identifier ~ (lp ~ rp ~ "->" ~> tfRetType) ^^ { case (id ~ typ) => FunctionHeader(typ, id, List()) }
  lazy val tfFunctionBody: PackratParser[FunctionBody] = tfBlock ^^ { b => FunctionBody(b) }

  lazy val tfNewArray: PackratParser[NewArray] = {
    "var" ~ tfSymbol ~ ":" ~ tfArrayTyp ~ "=" ~ tfArrayInit ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewArray(id, typ, init)
    } |
      "var" ~ tfSymbol ~ ":" ~ tfArrayTyp ^^ { _ => throw new ApdlParserException("Uninitialised array") }
  }

  lazy val tfNewVar: PackratParser[NewVar] = {
    "var" ~ tfSymbol ~ ":" ~ tfPrimitivesTyp ~ "=" ~ tfExpr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVar(id, typ, Some(init))
    } |
      "var" ~ tfSymbol ~ ":" ~ tfPrimitivesTyp ^^ {
        case (_ ~ id ~ _ ~ typ) => NewVar(id, typ, None)
      }
  }

  lazy val tfArrayInit: PackratParser[ArrayInit] = {
    "[" ~> tfLiteral <~ "]" ^^ ArrayInitCapacity |
      "{" ~> tfExpr ~ rep("," ~> tfExpr) <~ "}" ^^ { case (e ~ es) =>
        ArrayInitValue(e :: es)
      }
  }

  lazy val tfNewVal: PackratParser[NewVal] = {
    "val" ~ tfSymbol ~ ":" ~ tfPrimitivesTyp ~ "=" ~ tfExpr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVal(id, typ, init)
    }
  }

  lazy val tfSelectionStatement: PackratParser[Statement] = tfIfThenElse | tfIfThen
  lazy val tfIfThenElse: PackratParser[IfThenElse] = "if" ~ lp ~ tfExpr ~ rp ~ tfStatement ~ "else" ~ tfStatement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ trueBranch ~ _ ~ falseBranch) => IfThenElse(cond, trueBranch, falseBranch)
  }
  lazy val tfIfThen: PackratParser[IfThen] = "if" ~ lp ~ tfExpr ~ rp ~ tfStatement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => IfThen(cond, statement)
  }

  lazy val tfJump: PackratParser[Statement] = {
    tfBreak | tfContinue | tfReturn
  }

  lazy val tfReturn: PackratParser[Return] = "return" ~> tfExpr ^^ { expr => Return(expr) }
  lazy val tfBreak: PackratParser[Break] = "break" ^^ { _ => Break() }
  lazy val tfContinue: PackratParser[Continue] = "continue" ^^ { _ => Continue() }

  lazy val lp = "("
  lazy val rp = ")"
  lazy val lb = "{"
  lazy val rb = "}"
}
