import org.scalatest.FunSuite
import spl._
import prop._

object Visitor {
  var count = 0

  def reset() = count = 0

  def head (xs:List[Int]) : Int = xs.head
  def tail (xs:List[Int]) : List[Int] = xs.tail
  def length(xs:List[Int]) : Int = xs.length
  def map(f:Int => Int)(xs:List[Int]) : List[Int] = xs.map(f)
  def foo(x: Int) : Int = {
    count += 1
    return x
  }

  val lifted_head = SPL.mkVarT(head _)
  val lifted_tail = SPL.mkVarT(tail _)
  val lifted_map = SPL.mkVarT(map _)
  val lifted_foo = SPL.mkVarT(foo _)

  def visit(xs : SPL.Var[List[Int]]) : Int = {
    SPL.apply2(lifted_map, lifted_foo, xs)
    val ret = count
    reset()
    return ret
  }
}

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

  val a = SPL.mkVarT(1)
  val b = SPL.mkVarT(2)
  val c = SPL.mkVarT(3)
  val d = SPL.mkVarT(4)

  def plus (x : Int) (y : Int) : Int = x + y
  def cons (x: Int) (xs:List[Int]) : List[Int] = x :: xs

  test("plus") {
    val lifted_plus = SPL.mkVarT(plus _)
    val result = SPL.apply2(lifted_plus, x, y)
    assert(result == List((12, pq), (2, p_q), (-19,_pq), (-11,_p_q)))
  }

  test("cons") {
    val lifted_cons = SPL.mkVarT(cons _)

    val l0 = SPL.mkVarT(Nil)
    val l1 = SPL.apply2(lifted_cons, a, l0)
    val l2 = SPL.apply2(lifted_cons, b, l1)
    val l3 = SPL.apply2(lifted_cons, c, l2)
    val l4 = SPL.apply2(lifted_cons, d, l3)
    val l5 = SPL.apply2(lifted_cons, x, l4)

    val ret = Visitor.visit(l5)
    assert(ret == 20)
  }
}

