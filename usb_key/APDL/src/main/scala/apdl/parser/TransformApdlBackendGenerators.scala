package apdl.parser

trait TransformApdlBackendGenerators {

  def toApdlCode(tfTyp: TfRetTyp): String = tfTyp match {
    case TfFloat => "float"
    case TfInt => "int"
    case TfDouble => "double"
    case TfLong => "long"
    case TfBoolean => "bool"
    case TfChar => "char"
    case TfArray(typ) => s"${toApdlCode(typ)}[]"
    case TfByte => "byte"
    case TfShort => "short"
    case TfVoid => "void"
  }

  def toApdlCode(expr: Expr): String = expr match {
    case Add(left, right) => s"(${toApdlCode(left)} + ${toApdlCode(right)})"
    case Mul(left, right) => s"(${toApdlCode(left)} * ${toApdlCode(right)})"
    case Sub(left, right) => s"(${toApdlCode(left)} - ${toApdlCode(right)})"
    case Div(left, right) => s"(${toApdlCode(left)} / ${toApdlCode(right)})"
    case Cast(tfTyp, ex) => s"((${toApdlCode(tfTyp)})${toApdlCode(ex)})"
    case Literal(value) => s"$value"
    case Symbol(name) => s"$name"
    case FunctionCall(funcName, args) => s"$funcName(${args map toApdlCode mkString ","})"
    case ArrayAccess(array, field) => s"${toApdlCode(array)}[${toApdlCode(field)}]"
    case VarAssignement(target, value) => s"${toApdlCode(target)} = ${toApdlCode(value)}"
    case True() => s"true"
    case False() => s"false"
    case Or(left, right) => s"(${toApdlCode(left)} || ${toApdlCode(right)})"
    case And(left, right) => s"(${toApdlCode(left)} && ${toApdlCode(right)})"
    case Not(booleanExpr) => s"(!${toApdlCode(booleanExpr)})"
    case Greater(left, right) => s"(${toApdlCode(left)} > ${toApdlCode(right)})"
    case Smaller(left, right) => s"(${toApdlCode(left)} < ${toApdlCode(right)})"
    case GreaterEquals(left, right) => s"(${toApdlCode(left)} >= ${toApdlCode(right)})"
    case SmallerEquals(left, right) => s"(${toApdlCode(left)} <= ${toApdlCode(right)})"
    case Equals(left, right) => s"(${toApdlCode(left)} == ${toApdlCode(right)})"
    case NotEquals(left, right) => s"(${toApdlCode(left)} != ${toApdlCode(right)})"
  }


  def toApdlCode(statement: Statement): String = statement match {
    case While(cond, stat) => s"while(${toApdlCode(cond)})${toApdlCode(stat)}"
    case DoWhile(cond, stat) => s"do ${toApdlCode(stat)} while(${toApdlCode(cond)})"
    case IfThenElse(cond, trueBranch, falseBranch) => s"if(${toApdlCode(cond)}) ${toApdlCode(trueBranch)} else ${toApdlCode(falseBranch)}"
    case IfThen(cond, ifTrue) => s"if(${toApdlCode(cond)}) ${toApdlCode(ifTrue)}"
    case Return(expr) => s"return ${toApdlCode(expr)}"
    case Break() => s"break"
    case Continue() => s"continue"
    case Block(statements) =>
      s"""
         |{
         |  ${statements map toApdlCode mkString "\n"}
         |}
      """.stripMargin
    case ExpressionStatement(expression) => s"${toApdlCode(expression)}"
    case decl: Declaration => decl match {
      case FunctionDecl(FunctionHeader(resultType, identifier, parameters), FunctionBody(body)) =>
        s"""
           | def $identifier (${parameters map toApdlCode mkString ","}) -> ${toApdlCode(resultType)}
           |  ${toApdlCode(body)}
        """.stripMargin
      case NewVal(symbol, typ, init) => s"val ${toApdlCode(symbol)} : ${toApdlCode(typ)} = ${toApdlCode(init)}"
      case NewVar(symbol, typ, init) => s"val ${toApdlCode(symbol)} : ${toApdlCode(typ)}" + s"${
        init match {
          case Some(value) => s" = ${toApdlCode(value)}"
          case None => ""
        }
      }"
      case NewArray(symbol, typ, init) => s"${toApdlCode(symbol)} ${toApdlCode(typ)} = ${toApdlCode(init)}"
    }
  }
  def toApdlCode(typedIdentifier: TypedIdentifier): String = s"${typedIdentifier.name}:${toApdlCode(typedIdentifier.typ)}"

  def toApdlCode(init: ArrayInit): String = init match {
    case ArrayInitValue(values) => s"{${values map toApdlCode mkString ","}}"
    case ArrayInitCapacity(capacity) => s"[${toApdlCode(capacity)}]"
  }
}