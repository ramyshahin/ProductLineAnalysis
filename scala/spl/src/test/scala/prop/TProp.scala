import org.scalatest.FunSuite
import prop._

class PropSuite extends FunSuite {
  val vars = Config.mkVars(List("a", "b", "c"))
  val a = vars(0)
  val b = vars(1)
  val c = vars(2)

  test("single variable") {
    assert(a.isSAT())
    assert(b.isSAT())
    assert(c.isSAT())
  }

  test("and") {
    assert(And(a,b).isSAT())
  }

  test("or") {
    assert(Or(a,b).isSAT())
  }

  test("excluded middle") {
    assert(Or(a, Neg(a)).isSAT())
  }

  test("a and not a") {
    assert(And(a, Neg(a)).isSAT() == false)
  }
}
