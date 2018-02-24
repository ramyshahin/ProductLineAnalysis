package spl

import prop._

object SPL {
  type PresenceCondition = Prop

  type Val[T] = (T, PresenceCondition)

  type Var[T] = List[Val[T]]

  def mkVarT[T](a : T) : Var[T] = List((a, True()))

  def normalize[A](xs : Var[A]) : Var[A] = {
    xs.map(x => {
      val vs = xs.filter(v => x != v && x._1 == v._1)
      if (vs == Nil) x
      else {
        val h = vs.head
        (x._1, Or(x._2, h._2))
      }
    })
  }

  def apply[A,B](f : Var[A => B], a : Var[A]) : Var[B] = {
    val vs = for (_f <- f; _a <- a; pc = And(_f._2, _a._2); if pc.isSAT()) yield (_f._1(_a._1), pc)
    //normalize(vs)
    return vs
  }

  def apply2[A,B,C](f : Var [A => B => C], a : Var[A], b: Var[B]) : Var[C] = {
    apply(apply(f, a), b)
  }

}

sealed abstract class Lifted_List[T] {
  def length() : SPL.Var[Int] = {
    def plus(x:Int) (y:Int): Int = x + y
    val lifted_plus = SPL.mkVarT(plus _)
    this match {
      case Lifted_Nil() => SPL.mkVarT(0)
      case Lifted_Cons(x, xs) => SPL.apply2(lifted_plus, xs.length(), SPL.mkVarT(1))
    }
  }

  def head : SPL.Var[T] = {
    this match {
      case Lifted_Nil() => Nil
      case Lifted_Cons(x, xs) => x
    }
  }
}

case class Lifted_Nil[T]() extends Lifted_List[T]
case class Lifted_Cons[T](x: SPL.Var[T], xs: Lifted_List[T]) extends Lifted_List[T]
