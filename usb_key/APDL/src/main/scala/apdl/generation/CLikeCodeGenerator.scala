package apdl.generation

import java.io.File

import apdl.ApdlFramework.{Arduino, Mbed}
import apdl.ApdlUtils._
import apdl._
import apdl.parser.{ApdlType, _}

import scala.Function._

class CLikeCodeGenerator(project: ApdlProject, device: ApdlDevice)(implicit val config: ApdlConfig) {

  private val symbolTable: SymbolTable = new SymbolTable
  private val defines: List[ApdlDefine] = project.defineInputs ::: project.defineTransforms ::: project.defineComponents
  private val transformCodeGen: CLikeTransformCodeGenerator = new CLikeTransformCodeGenerator
  private val framework: ApdlFramework = ApdlFramework.valueOf(device.framework) match {
    case Some(value) => value
    case None => throw new ApdlProjectException(s"Unknow framework ${device.framework}")
  }


  def mkDevice(srcDir: File): Unit = {
    val ext = fileExtension(device.framework)
    val mainFile = new File(srcDir, s"${project.name}_name.$ext")
    if (!mainFile.createNewFile())
      throw new ApdlDirectoryException(s"Can't create file ${mainFile.getAbsolutePath}")
    debug(s"create file ${mainFile.getAbsolutePath}")
    val mainPw = ApdlPrintWriter.getPw(framework)(mainFile)
    debug(s"Generate inputs for device ${device.name}")
    generateInputs(mainPw)
    debug(s"Generate the components")
    generateComponents(mainPw)
    debug(s"Generate serials for device ${device.name}")
    generateSerials(mainPw)
    mainPw.close()
  }

  def fileExtension(framework: String): String = framework match {
    case "arduino" => "ino"
    case "mbed" => "cpp"
    case _ => throw new ApdlCodeGenerationException(s"Unknow framework : $framework")
  }

  def componentSymbols(args: List[String], defineParameters: List[Parameter], inputs: List[Parameter]): Map[String, String] = {
    val params = defineParameters.map(_.id) ::: inputs.map(i => transformCodeGen(i.typ))
    require(args.length == params.length, s"parameter size and arguments size are not the same : ${args.length} != ${params.length}")
    ((args zip params) map tupled((s, p) => (p, s))).toMap
  }

  def zipArgWithIdentifier(args: List[String], defineParameters: List[Parameter], inputsParameters: List[Parameter]): Map[String, String] = {
    val params = defineParameters ::: inputsParameters
    require(args.length == params.length, s"parameter size and arguments size are not the same : ${args.length} != ${params.length}")
    ((args zip params) map tupled((s, p) => (p.id, s))).toMap
  }

  def generateInputs(out: ApdlPrintWriter): Unit = {
    // Generate default input
    debug(s"\tGenerate default inputs")
    generateDefaultInputs(out)
    // Make the symbolTable for non-default input
    debug(s"\tGenerate non default inputs")
    generateNonDefaultInputs(out)
  }

  def generateNonDefaultInputs(out: ApdlPrintWriter): Unit = {
    val inputs = device.inputs
    val nonDefaultInputs = inputs.filter(isNonDefault)

    // For each inputs, take the ones who are generable
    def process(inputs: List[ApdlInput]): Unit = if (inputs.nonEmpty) {
      val (generableInputs, nonGenerableInputs) = inputs.partition(isGenerable)
      generableInputs.foreach { i =>
        if (isTransform(i)) {
          val sourceInput = symbolTable.get(i.args.head) match {
            case default: InputDefault => default
            case transformed: InputTransformed => transformed
            case componented: InputComponented => componented
            case _ => throw new ApdlProjectException(s"Can't find source input for input ${i.identifier}")
          }
          val definition = defines.find(_.identifier == i.defineInputIdentifier) match {
            case Some(value) => value
            case None => throw new ApdlProjectException(s"Unknow define for input ${i.identifier}")
          }
          assume(definition.isInstanceOf[ApdlDefineTransform])

          // code generation
          val functionDecl = definition.asInstanceOf[ApdlDefineTransform].functionDecl

          if (!symbolTable.contains(functionDecl.header.identifier)) {
            // A transform is a function
            out.printlnFunction {
              s"""
                 | // Transform ${functionDecl.header.identifier}
                 | ${transformCodeGen(functionDecl)}
                 | // End transform ${functionDecl.header.identifier}
             """.stripMargin
            }
            // Add the transform into the symbol table
            symbolTable.add(functionDecl.header.identifier, Transform(functionDecl))
          }
          symbolTable.add(i.identifier, InputTransformed(i.identifier, definition.asInstanceOf[ApdlDefineTransform], sourceInput))

        } else if (isComponented(i)) {

          val definition = defines.find(_.identifier == i.defineInputIdentifier) match {
            case Some(value) => value
            case None => throw new ApdlProjectException(s"Unknow define for input ${i.identifier}")
          }

          assume(definition.isInstanceOf[ApdlDefineComponent])

          val component = definition.asInstanceOf[ApdlDefineComponent]

          val nbNonInputs = component.parameters.length
          val args = i.args.drop(nbNonInputs)
          val sourceInputs: List[Input] = symbolTable.gets(args).map {
            case default: InputDefault => default
            case transformed: InputTransformed => transformed
            case componented: InputComponented => componented
            case _ => throw new ApdlProjectException(s"Can't find source input for input ${i.identifier}")
          }


          symbolTable.add(i.identifier, InputComponented(i.identifier, definition.asInstanceOf[ApdlDefineComponent], sourceInputs))

        } else {
          // something is wrong...
          throw new ApdlProjectException(s"Input ${i.identifier} from device ${device.name} encountered some problems")
        }
      }
      process(nonGenerableInputs)
    }

    process(nonDefaultInputs)
  }

  def isTransform(input: ApdlInput): Boolean = defines.find {
    case ApdlDefineTransform(functionDecl) => functionDecl.header.identifier == input.defineInputIdentifier
    case _ => false
  } match {
    case Some(value) =>
      assert(value.isInstanceOf[ApdlDefineTransform])
      assert(input.args.length == 1)
      true
    case None => false
  }

  def isComponented(input: ApdlInput): Boolean = defines.find {
    case component: ApdlDefineComponent => component.identifier == input.defineInputIdentifier
    case _ => false
  } match {
    case Some(value) =>
      assert(value.isInstanceOf[ApdlDefineComponent])
      true
    case None => false
  }

  def isGenerable(input: ApdlInput): Boolean = {
    if (isTransform(input)) {
      assert(input.args.length == 1)
      val sourceInput = input.args.head
      symbolTable.contains(sourceInput)
    }
    else if (isComponented(input)) {
      val componentDefine = defines.find(_.identifier == input.defineInputIdentifier) match {
        case Some(value) => value match {
          case component: ApdlDefineComponent => component
          case _ => throw new ApdlProjectException(s"Unexpected definition found for input ${input.identifier} from device ${device.name}")
        }
        case None => throw new ApdlProjectException(s"Unknow define component for input ${input.identifier} from device ${device.name}")
      }
      val nbNonInputs = componentDefine.parameters.length
      val args = input.args.drop(nbNonInputs)
      args.forall(symbolTable.contains)
    }
    else {
      throw new ApdlProjectException(s"Wrong input type for input ${input.identifier} from device ${device.name}")
    }
  }

  def isNonDefault(input: ApdlInput): Boolean = {
    !isDefault(input)
  }

  def isDefault(input: ApdlInput): Boolean = {
    defines.find(_.identifier == input.defineInputIdentifier) match {
      case Some(definition) => definition match {
        case _: ApdlDefineInput => true
        case _: ApdlDefineComponent => false
        case _: ApdlDefineTransform => false
      }
      case None =>
        throw new ApdlProjectException(s"Unknow type for input ${input.identifier}")
    }
  }

  // Generate the default input inside the symbol table
  def generateDefaultInputs(out: ApdlPrintWriter): Unit = device.inputs.foreach { input =>
    defines.find(_.identifier == input.defineInputIdentifier) match {
      case Some(definition) => definition match {
        case ApdlDefineInput(name, parameters, gens) =>
          // A default input

          val gen = gens.getOrElse(framework.identifier, throw new ApdlProjectException(s"Unknow framework $framework for input definition : $name"))

          implicit val args = zipArgWithIdentifier(input.args, parameters, List()) + ("id" -> IdGenerator.nextVariable(input.identifier))

          out.printlnGlobal(gen.global.replaceWithArgs)
          out.printlnSetup(gen.setup.replaceWithArgs)
          out.printlnLoop(gen.loop.replaceWithArgs)

          symbolTable.add(input.identifier, InputDefault(
            input.identifier,
            definition.asInstanceOf[ApdlDefineInput],
            input.args,
            gen.expr.replaceWithArgs))
        case _ =>
      }
      case None => throw new ApdlProjectException(s"Unknow type for input ${input.identifier}")
    }
  }

  def generateComponents(out: ApdlPrintWriter): Unit = device.inputs.foreach { input =>
    val comp: ApdlDefine = defines.find(_.identifier == input.defineInputIdentifier) match {
      case Some(value) => value
      case None => throw new ApdlProjectException(s"Unknow definition for input ${input.identifier} on device ${device.name}")
    }
    //noinspection TypeCheckCanBeMatch
    if (comp.isInstanceOf[ApdlDefineComponent]) {
      val component = comp.asInstanceOf[ApdlDefineComponent]
      // Generate the component
      if (!symbolTable.contains(component.identifier)) {
        // A component is like a function
        val retTyp = transformCodeGen(component.outputType.outputType)

        implicit val args: Map[String, String] = componentSymbols(input.args, component.parameters, component.inputs.parameters)
        val gen = component.gens.getOrElse(framework.identifier,
          throw new ApdlProjectException(s"Unknow framework ${framework.identifier} for input ${input.identifier} on device ${device.name}"))

        out.printlnSetup(gen.setup.replaceWithArgs.removeSymbol())
        out.printlnLoop(gen.loop.replaceWithArgs.removeSymbol())
        out.printlnGlobal(gen.global.replaceWithArgs.removeSymbol())

        val expr = gen.expr.replaceWithArgs.removeSymbol()
        val parameters = component.inputs.parameters.map(p => s"${transformCodeGen(p.typ)} ${p.id}").mkString(",")
        val id = s"component_${component.identifier}_${input.identifier}"

        out.printlnFunction(
          s"""
             |// Component ${component.identifier}
             |$retTyp $id($parameters) {
             |  return $expr;
             |}
             |// End of ${component.identifier}
               """.stripMargin)
      }
    }
  }

  def generateArduinoSerials(out: ApdlPrintWriter): Unit = device.serials.foreach { serial =>
    // Generate callback function
    val input = symbolTable.getOption(serial.inputName) match {
      case Some(value) => value match {
        case input: Input => input
        case _ => throw new ApdlProjectException(s"Unknow input name ${serial.inputName} for device ${device.name}")
      }
      case None => throw new ApdlProjectException(s"Unknow input name ${serial.inputName} for device ${device.name}")
    }

    val dataType = input.getDefinition match {
      case ApdlDefineInput(name, _, gens) =>
        gens.getOrElse(framework.identifier, throw new ApdlCodeGenerationException(s"Unknow type for input $name")).typ match {
          case Some(value) => value
          case None => throw new ApdlCodeGenerationException(s"Unknow type for input $name")
        }
      case ApdlDefineComponent(_, _, _, outputType, _) =>

        outputType.outputType
      case ApdlDefineTransform(functionDecl) =>
        functionDecl.header.resultType.asApdlType
    }

    val expr = getExpr(input)
    val callbackIdentifier = s"serial_${serial.inputName}"

    serial.sampling match {
      case ApdlSamplingUpdate =>
        val typ: String = transformCodeGen(dataType)
        out.printlnGlobal(s"$typ last_$callbackIdentifier;")
        out.printlnSetup(s"last_$callbackIdentifier = $expr;")
        out.printlnFunction {
          s"""
             |void $callbackIdentifier(){
             |  // Get data
             |  $typ data = $expr;
             |  if(data != last_$callbackIdentifier) {
             |    char buffer[1024];
             |    sprintf(buffer,"${serial.inputName} : %d", (int)data);
             |    //sprintf(buffer,"${serial.inputName} : ${transformCodeGen.strTypeFormater(dataType)}", data);
             |    Serial.println(buffer);
             |  }
             |  last_$callbackIdentifier = data;
             |}
       """.stripMargin
        }
        out.printlnSetup(s"t.every(1000,$callbackIdentifier);")
      case timer: ApdlSamplingTimer =>
        out.printlnFunction {
          s"""
             |void $callbackIdentifier(){
             |  // Get data
             |  ${transformCodeGen(dataType)} data = $expr;
             |  char buffer[1024];
             |  sprintf(buffer,"${serial.inputName} : %d", (int)data);
             |  //sprintf(buffer,"${serial.inputName} : ${transformCodeGen.strTypeFormater(dataType)}", data);
             |  Serial.println(buffer);
             |}
       """.stripMargin
        }

        out.printlnSetup(s"t.every(${timer.ms},$callbackIdentifier);")
    }
  }

  def getExpr(input: Input): String = input match {
    case InputDefault(_, _, _, expr) => expr
    case InputTransformed(_, define, sourceInput) =>
      val transformIdentifier = define.functionDecl.header.identifier
      s"$transformIdentifier(${getExpr(sourceInput)})"
    case InputComponented(id, define, sourceInputs) =>
      val identifier = s"component_${define.identifier}_$id"
      s"$identifier(${sourceInputs map getExpr mkString ","})"
  }

  def generateMbedSerials(out: ApdlPrintWriter): Unit = device.serials.foreach { serial =>
    // Generate callback function
    val input = symbolTable.getOption(serial.inputName) match {
      case Some(value) => value match {
        case input: Input => input
        case _ => throw new ApdlProjectException(s"Unknow input name ${serial.inputName} for device ${device.name}")
      }
      case None => throw new ApdlProjectException(s"Unknow input name ${serial.inputName} for device ${device.name}")
    }

    val dataType = input.getDefinition match {
      case ApdlDefineInput(name, _, gens) =>
        gens.getOrElse(framework.identifier, throw new ApdlCodeGenerationException(s"Unknow type for input $name")).typ match {
          case Some(value) => value
          case None => throw new ApdlCodeGenerationException(s"Unknow type for input $name")
        }
      case ApdlDefineComponent(_, _, _, outputType, _) =>

        outputType.outputType
      case ApdlDefineTransform(functionDecl) =>
        functionDecl.header.resultType.asApdlType
    }

    val expr = getExpr(input)
    val callbackIdentifier = s"serial_${serial.inputName}"

    serial.sampling match {
      case ApdlSamplingUpdate =>
        val typ: String = transformCodeGen(dataType)
        out.printlnGlobal(s"$typ last_$callbackIdentifier;")
        out.printlnSetup(s"last_$callbackIdentifier = $expr;")
        out.printlnFunction {
          s"""
             |void $callbackIdentifier(){
             |  // Get data
             |  $typ data = $expr;
             |  if(data != last_$callbackIdentifier) {
             |    pc.printf("${serial.inputName} : ${transformCodeGen.strTypeFormater(dataType)}",data);
             |    last_$callbackIdentifier = data;
             |  }
             |}
             """.stripMargin
        }
        out.printlnSetup(s"ticker.attach(&$callbackIdentifier,1.0);")
      case timer: ApdlSamplingTimer =>
        out.printlnFunction {
          s"""
             |void $callbackIdentifier(){
             |  // Get data
             |  ${transformCodeGen(dataType)} data = $expr;
             |  pc.printf("${serial.inputName} : ${transformCodeGen.strTypeFormater(dataType)}",data);
             |}
             """.stripMargin
        }
        out.printlnSetup(s"ticker.attach(&$callbackIdentifier,${timer.s}.0);")
    }
  }

  def generateSerials(out: ApdlPrintWriter): Unit = framework match {
    case Arduino => generateArduinoSerials(out)
    case Mbed => generateMbedSerials(out)
  }

  implicit class ApdlScriptString(string: String) {
    def replaceWithArgs(implicit args: Map[String, String]): String = {
      def inner(arguments: Map[String, String], acc: String): String = {
        if (arguments.isEmpty)
          acc
        else {
          val (id, value) = arguments.head
          inner(arguments.tail, acc.replace("@" + id, value))
        }
      }

      inner(args, string)
    }

    def removeSymbol(symbol: String = "@"): String = {
      string.replaceAll("@", "")
    }
  }
}

class CLikeTransformCodeGenerator {
  def apply(apdlAst: ApdlAst): String = apdlAst match {
    case e: Expr => e match {
      case Add(left, right) => s"(${apply(left)} + ${apply(right)})"
      case Mul(left, right) => s"(${apply(left)} * ${apply(right)})"
      case Sub(left, right) => s"(${apply(left)} - ${apply(right)})"
      case Div(left, right) => s"(${apply(left)} / ${apply(right)})"
      case Cast(tfTyp, expr) => s"((${apply(tfTyp)})${apply(expr)})"
      case Literal(value) => s"$value"
      case Symbol(name) => s"$name"
      case FunctionCall(funcName, args) => s"$funcName(${args map apply mkString ","})"
      case ArrayAccess(array, field) => s"${apply(array)}[${apply(field)}]"
      case VarAssignement(target, value) => s"${apply(target)} = ${apply(value)}"
      case True() => s"1"
      case False() => s"0"
      case Or(left, right) => s"(${apply(left)} || ${apply(right)})"
      case And(left, right) => s"(${apply(left)} && ${apply(right)})"
      case Not(booleanExpr) => s"(! ${apply(booleanExpr)})"
      case Greater(left, right) => s"(${apply(left)} > ${apply(right)})"
      case Smaller(left, right) => s"(${apply(left)} < ${apply(right)})"
      case GreaterEquals(left, right) => s"(${apply(left)} >= ${apply(right)})"
      case SmallerEquals(left, right) => s"(${apply(left)} <= ${apply(right)})"
      case Equals(left, right) => s"(${apply(left)} == ${apply(right)})"
      case NotEquals(left, right) => s"(${apply(left)} != ${apply(right)})"
    }
    case t: TfRetTyp => t match {
      case TfInt => "int"
      case TfLong => "long"
      case TfByte => "byte"
      case TfShort => "short"
      case TfChar => "char"
      case TfDouble => "double"
      case TfFloat => "float"
      case TfVoid => "void"
      case TfBoolean => "bool"
      case TfArray(typ: TfTyp) => s"${apply(typ)}*"
    }
    case TypedIdentifier(name, typ) => s"${apply(typ)} $name"
    case s: Statement => s match {
      case While(cond, statement) =>
        s"""
           |while(${apply(cond)})
           |  ${apply(statement)}
         """.stripMargin
      case DoWhile(cond, statement) =>
        s"""
           |do
           |  ${apply(statement)}
           |while (${apply(cond)});
         """.stripMargin
      case IfThenElse(cond, trueBranch, falseBranch) =>
        s"""
           |if(${apply(cond)})
           |  ${apply(trueBranch)}
           |else
           |  ${apply(falseBranch)}
         """.stripMargin
      case IfThen(cond, ifTrue) =>
        s"""
           |if(${apply(cond)})
           |  ${apply(ifTrue)}
         """.stripMargin
      case Return(expr) => s"return ${apply(expr)};"
      case Break() => s"break;"
      case Continue() => s"continue;"
      case Block(statements) =>
        s"""|{
            |  ${statements map apply mkString "\n"}
            |}""".stripMargin
      case ExpressionStatement(expression) => s"${apply(expression)};"
      case decl: Declaration => decl match {
        // No { or } because the body is always a block and the block already generate the {}
        case FunctionDecl(header, body) =>
          s"""
             |${apply(header.resultType)} ${header.identifier} (${header.parameters map apply mkString ","})
             |  ${apply(body.body)}
           """.stripMargin
        case NewVal(symbol, typ, init) => s"${apply(typ)} ${apply(symbol)} = ${apply(init)};"
        case NewVar(symbol, typ, init) => init match {
          case Some(value) => s"${apply(typ)} ${apply(symbol)} = ${apply(value)};"
          case None => s"${apply(typ)} ${apply(symbol)};"
        }
        case NewArray(symbol, typ, init) => s"${apply(typ)} ${apply(symbol)} = ${apply(init)};"
      }
    }
    case arrayInit: ArrayInit => arrayInit match {
      case ArrayInitValue(values) => s"{${values map apply mkString ","}"
      case ArrayInitCapacity(capacity) => s"[${apply(capacity)}]"
    }
  }

  def apply(typ: ApdlType): String = typ match {
    case ApdlType.Int => "int"
    case ApdlType.Float => "float"
    case ApdlType.Long => "long"
    case ApdlType.Bool => "bool"
    case ApdlType.Double => "double"
    case ApdlType.Short => "short"
    case ApdlType.Char => "char"
    case ApdlType.Byte => "byte"
    case _ => throw new ApdlCodeGenerationException(s"Unsupported ApdlType : $typ")
  }

  def strTypeFormater(typ: ApdlType): String = typ match {
    case ApdlType.Int => "%d"
    case ApdlType.Float => "%f"
    case ApdlType.Long => "%l"
    case ApdlType.Bool => "%d"
    case ApdlType.Double => "%f"
    case ApdlType.Short => "%d"
    case ApdlType.Char => "%c"
    case ApdlType.Byte => "%c"
    case _ => throw new ApdlCodeGenerationException(s"Unsupported ApdlType : $typ")
  }
}