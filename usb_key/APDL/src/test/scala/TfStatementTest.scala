import apdl.ApdlParserException
import apdl.parser._
import org.scalatest.FlatSpec

import scala.util.parsing.input.CharSequenceReader

class TfStatementTest extends FlatSpec {

  val parser = new TransformDslParser {}

  import parser._

  def assertAst(code: String, expected: Statement): Unit = {
    val result = parser.parse(tfStatement, new PackratReader(new CharSequenceReader(code))) match {
      case Success(r, _) => r
      case error: NoSuccess =>
        println(s"can't parse $b -> $error")
        fail()
    }

    s"$code" should s"produce $expected" in {
      assert(result == expected)
    }
  }

  val i = Symbol("i")
  val j = Symbol("j")
  val n = Symbol("n")
  val a = Symbol("a")
  val b = Symbol("b")
  val x = Symbol("x")
  val zero = Literal("0")
  val one = Literal("1")
  val two = Literal("2")
  val three = Literal("3")
  val five = Literal("5")
  val ten = Literal("10")

  assertAst("while(i > 0) i = i - 1", While(Greater(i, zero), ExpressionStatement(VarAssignement(i, Sub(i, 1)))))
  assertAst("while(i > 0 && j < 10) n = n * 2 + 1", While(And(Greater(i, zero), Smaller(j, ten)), ExpressionStatement(VarAssignement(n, Add(Mul(n, 2), 1)))))
  assertAst("while((true))i = i + 1",While(True(),ExpressionStatement(VarAssignement(i,Add(i,1)))))
  assertAst("while(((((((true)))))))i = i + 1",While(True(),ExpressionStatement(VarAssignement(i,Add(i,1)))))
  assertAst("do i = i + 1 while(true)", DoWhile(True(), ExpressionStatement(VarAssignement(i, Add(i, 1)))))
  assertAst(
    "if(a == b) print(a) else print(b)",
    IfThenElse(
      Equals(a, b),
      ExpressionStatement(FunctionCall("print", List(a))),
      ExpressionStatement(FunctionCall("print", List(b)))
    )
  )
  assertAst("if(true) return (-8.716613540504124E307 > false)",IfThen(True(),Return(Greater(Literal("-8.716613540504124E307"),False()))))

  assertAst("while((int)3.4){(double)4 break}",
    While(
      Cast(
        TfInt,
        Literal("3.4")
      ), Block(List(
        ExpressionStatement(Cast(TfDouble, Literal("4"))),
        Break()
      ))
    ))

  assertAst("i = 5", ExpressionStatement(VarAssignement(i, Literal("5"))))
  assertAst("i = 5.12", ExpressionStatement(VarAssignement(i, Literal("5.12"))))
  assertAst("i = i + 1", ExpressionStatement(VarAssignement(i, Add(i, 1))))
  assertAst("i = j > 1 || b < 1", ExpressionStatement(VarAssignement(i, Or(Greater(j, one), Smaller(b, one)))))
  assertAst("if (a || b < 5) x = log(b)", IfThen(Or(a, Smaller(b, five)), ExpressionStatement(VarAssignement(x, FunctionCall("log", List(b))))))
  assertAst("var j : int = i + 1", NewVar(j, TfInt, Some(Add(i, 1))))
  assertAst("var array : int[] = {1,2,3}", NewArray(Symbol("array"), TfArray(TfInt), ArrayInitValue(List(one, two, three))))
  assertAst("var array : float[] = [100]", NewArray(Symbol("array"), TfArray(TfFloat), ArrayInitCapacity(Literal("100"))))
  assertAst("var a : int", NewVar(a, TfInt, None))
  assertAst("var a : short", NewVar(a, TfShort, None))
  assertAst("var a : float", NewVar(a, TfFloat, None))
  assertAst("var a : double", NewVar(a, TfDouble, None))
  assertAst("var a : byte", NewVar(a, TfByte, None))
  assertAst("var a : char", NewVar(a, TfChar, None))
  assertAst("while(true)if(i > 10) break", While(True(), IfThen(Greater(Symbol("i"), Literal("10")), Break())))
  assertAst("while(true)if(i > 10) continue", While(True(), IfThen(Greater(Symbol("i"), Literal("10")), Continue())))
  assertAst("break", Break())
  assertAst("{continue break (true >= false) continue (!true) break}",Block(List(
    Continue(),
    Break(),
    ExpressionStatement(GreaterEquals(True(),False())),
    Continue(),
    ExpressionStatement(Not(True())),
    Break()
  )))
  assertAst("continue", Continue())
  assertAst("{{break i = i + 1}}", Block(List(Block(List(Break(), ExpressionStatement(VarAssignement(Symbol("i"), Add(Symbol("i"), Literal("1")))))))))
  assertAst("{break i = i + 1}", Block(List(Break(), ExpressionStatement(VarAssignement(Symbol("i"), Add(Symbol("i"), Literal("1")))))))
  assertAst("{i = i + 1 * 2 - 3 continue break i = i + 1}", Block(List(
    ExpressionStatement(VarAssignement(Symbol("i"), Sub(Add(Symbol("i"), Mul(Literal("1"), Literal("2"))), Literal("3")))),
    Continue(),
    Break(),
    ExpressionStatement(VarAssignement(Symbol("i"), Add(Symbol("i"), Literal("1"))))
  )))
  assertAst("{a[2] = 3 break continue break break continue}", Block(List(
    ExpressionStatement(VarAssignement(ArrayAccess(Symbol("a"), Literal("2")), Literal("3"))),
    Break(),
    Continue(),
    Break(),
    Break(),
    Continue()
  )))

  assertAst("array[9] = 10", ExpressionStatement(VarAssignement(ArrayAccess(Symbol("array"), Literal("9")), Literal("10"))))
  assertAst("array[i] = i", ExpressionStatement(VarAssignement(ArrayAccess(Symbol("array"), Symbol("i")), Symbol("i"))))

  assertThrows[ApdlParserException] {
    assertAst("var a : int[]", NewVar(a, TfArray(TfInt), None))
  }

  assertThrows[ApdlParserException] {
    assertAst(
      """
        |var a : int[]
        |var b : float = 3.2
      """.stripMargin, NewVar(a, TfArray(TfInt), None))
  }

  assertAst("def fac (x:int) -> int {if(x < 2) return 1 else return x * fac(x-1)}",
    FunctionDecl(
      FunctionHeader(TfInt, "fac", List(TypedIdentifier("x", TfInt))),
      FunctionBody(Block(List(IfThenElse(
        Smaller(Symbol("x"), Literal("2")),
        Return(Literal("1")),
        Return(Mul(Symbol("x"), FunctionCall("fac", List(Sub(Symbol("x"), Literal("1"))))))
      ))))
    )
  )

  assertAst("def one () -> int {return 1}",
    FunctionDecl(
      FunctionHeader(TfInt, "one", List()),
      FunctionBody(Block(List(Return(Literal("1")))))
    )
  )

  assertAst(
    """
      |def fac(x:int) -> int {
      | def fac_inner(x:int,acc:int) -> int {
      |   if(x < 2) return acc else return fac_inner(x-1,acc*x)
      | }
      | return fac_inner(x,1)
      |}
    """.stripMargin, FunctionDecl(
      FunctionHeader(TfInt, "fac", List(TypedIdentifier("x", TfInt))),
      FunctionBody(Block(List(
        FunctionDecl(
          FunctionHeader(TfInt, "fac_inner", List(
            TypedIdentifier("x", TfInt),
            TypedIdentifier("acc", TfInt)
          )),
          FunctionBody(Block(List(IfThenElse(
            Smaller(Symbol("x"), Literal("2")),
            Return(Symbol("acc")),
            Return(FunctionCall("fac_inner", List(
              Sub(Symbol("x"), Literal("1")),
              Mul(Symbol("acc"), Symbol("x"))
            )))
          ))))
        ),
        Return(FunctionCall("fac_inner", List(Symbol("x"), Literal("1"))))
      )))
    )
  )
}
