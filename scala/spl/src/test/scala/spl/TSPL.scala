import org.scalatest.FunSuite
import spl._
import prop._

class SPLSuite extends FunSuite {
  val vars = Config.mkVars(List("P", "Q", "R", "S"))
  val p = vars(0)
  val q = vars(1)
  val r = vars(2)
  val s = vars(3)

  val _p = Neg(p)
  val _q = Neg(q)
  val pq = And(p, q)
  val p_q = And(p, _q)
  val _pq = And(_p, q)
  val _p_q = And(_p, _q)

  val x = List((7, pq), (-3, p_q), (-8, _pq), (0, _p_q))
  val y = List((5, p), (-11, _p))
  val z = List((6, p))

  def plus (x : Int) (y : Int) : Int = x + y

  test("plus") {
    val lifted_plus = SPL.mkVarT(plus _)
    val result = SPL.apply2(lifted_plus, x, y)
    assert(result == List((12, pq), (2, p_q), (-19,_pq), (-11,_p_q)))
  }
}

