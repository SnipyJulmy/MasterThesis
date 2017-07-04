import apdl.parser.{DslApdlBackendGenerators, MainParsers}
import org.scalacheck.Prop._

class ProjectTest extends ApdlFlatSpec {
  val apdlProjectGenerators = new ApdlProjectGenerators(1, 1)
  val apdlCodeGenerator = new DslApdlBackendGenerators {}

  behavior of "MainParsers"

  it should "parse some correct apdl project" in {
    check {
      forAll(apdlProjectGenerators.genProject) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parser.program)
        ast == x
      }
    }
  }

  it should "parse some correct device" in {
    check {
      forAll(apdlProjectGenerators.genDevice) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parser.apdlDevice)
        ast == x
      }
    }
  }

  it should "parse some correct input" in {
    check {
      forAll(apdlProjectGenerators.genInput) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parser.apdlInput)
        ast == x
      }
    }
  }

  it should "parse some correct serial" in {
    check {
      forAll(apdlProjectGenerators.genSerial) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parser.apdlSerial)
        ast == x
      }
    }
  }

  it should "parse some correct sampling" in {
    check {
      forAll(apdlProjectGenerators.genSampling) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parser.apdlSampling)
        ast == x
      }
    }
  }

  it should "parse some correct time unit" in {
    check {
      forAll(apdlProjectGenerators.genTimeUnit) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parser.timeUnit)
        ast == x
      }
    }
  }
}
