package spl

import prop._

object SPL {
  type PresenceCondition = Prop

  type Val[T] = (T, PresenceCondition)

  type Var[T] = List[Val[T]]

  def mkVarT[T](a : T) : Var[T] = List((a, True()))

  def apply[A,B](f : Var[A => B], a : Var[A]) : Var[B] = {
    for (_f <- f; _a <- a; pc = And(_f._2, _a._2); if pc.isSAT()) yield (_f._1(_a._1), pc)
  }

  def apply2[A,B,C](f : Var [A => B => C], a : Var[A], b: Var[B]) : Var[C] = {
    apply(apply(f, a), b)
  }

}