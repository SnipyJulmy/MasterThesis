import apdl.parser._
import org.scalacheck.Prop._

class DefineTest extends ApdlFlatSpec {

  val apdlCodeGenerator = new DslApdlBackendGenerators {}

  import parser._

  behavior of "DefineParser"

  it should "Parse some correct defined component" in {
    check {
      forAllNoShrink(StringGenerators.defineComponentGen) { c =>
        parse(c, apdlDefine).isInstanceOf[ApdlDefineComponent]
      }
    }
  }

  it should "Parse some correct defined input" in {
    check {
      forAllNoShrink(StringGenerators.defineInputGen) { i =>
        parse(i, apdlDefine).isInstanceOf[ApdlDefineInput]
      }
    }
  }

  it should "Parse some correct defined @gen" in {
    check {
      forAllNoShrink(StringGenerators.genGen) { g =>
        val (_, genData) = parse(g, gen)
        genData.isInstanceOf[Gen]
      }
    }
  }

  it should "Parse some correct defined @in" in {
    check {
      forAllNoShrink(StringGenerators.inGen) { i =>
        parse(i, inputs).isInstanceOf[Inputs]
      }
    }
  }

  it should "Parse some correct defined @out" in {
    check {
      forAllNoShrink(StringGenerators.outGen) { o =>
        parse(o, output).isInstanceOf[Output]
      }
    }
  }

  val t1: String =
    """
      |@define component simpleOperator op:str {
      |    @in x:int y:int
      |    @out int
      |    @gen mbed {
      |        global = ""
      |        setup = ""
      |        loop = ""
      |        expr = "@x @op @y"
      |    }
      |    @gen arduino {
      |        global = ""
      |        setup = ""
      |        loop = ""
      |        expr = "@x @op @y"
      |    }
      |}
    """.stripMargin

  //noinspection VariablePatternShadow
  it should s"Produce the correct AST for $t1" in {
    val ast = parse(t1, apdlDefine)
    ast match {
      case ApdlDefineComponent(name, parameters, inputs, outputType, gens) =>
        assert(name == "simpleOperator")
        assert(parameters == List(Parameter("op", ApdlType.Str)))
        assert(inputs == Inputs(List(Parameter("x", ApdlType.Int), Parameter("y", ApdlType.Int))))
        assert(outputType == Output(ApdlType.Int))
        assert(gens == Map(
          "mbed" -> Gen("", "", "", "@x @op @y", None),
          "arduino" -> Gen("", "", "", "@x @op @y", None)
        ))
      case _ => fail("Should not produce something else than a define component")
    }
  }

  it should "Parse some correct AST transcoded to code" in {
    val apdlDefineGenerators = new ApdlDefineGenerators(3, 3)
    check {
      forAll(apdlDefineGenerators.typGen) { t =>
        val code = apdlCodeGenerator.toApdlCode(t)
        val ast = parse(code, apdlType)
        ast == t
      }
    }
    check {
      forAll(apdlDefineGenerators.genParameter) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parameter)
        ast == x
      }
    }
    check {
      forAll(apdlDefineGenerators.genGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, genBody)
        ast == x
      }
    }
    check {
      forAll(apdlDefineGenerators.genGens) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, gens)
        ast == x
      }
    }
    check {
      forAll(apdlDefineGenerators.inGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, inputs)
        ast == x
      }
    }
    check {
      forAll(apdlDefineGenerators.outGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, output)
        ast == x
      }
    }
    check {
      forAll(apdlDefineGenerators.genDefineComponent) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[ApdlDefine])
        val ast = parse(code, apdlDefine)
        ast match {
          case component: ApdlDefineComponent => component == x
          case _ => false
        }
      }
    }
    check {
      forAll(apdlDefineGenerators.genDefineInput) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[ApdlDefine])
        val ast = parse(code, apdlDefine)
        ast match {
          case input: ApdlDefineInput => input == x
          case _ => false
        }
      }
    }
    check {
      forAll(apdlDefineGenerators.genDefineTransform) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[ApdlDefine])
        val ast = parse(code, apdlDefine)
        ast == x
      }
    }
  }
}
