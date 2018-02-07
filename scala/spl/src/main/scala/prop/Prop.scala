package prop

import net.sf.javabdd.BDDFactory
import net.sf.javabdd.BDD

object Config {
	val factory = BDDFactory.init(100, 100)

	def mkVars(varNames: List[String]) : List[Prop] = {
    factory.setVarNum(varNames.length)
    (List.range(0, varNames.length), varNames).zipped.map((i, n) => Var(i, n))
  }
}

sealed abstract class Prop(b: BDD) {
  private def _b = b

  def negate : BDD = b.not
  def conj(that: Prop) : BDD = b.and(that._b)
  def disj(that: Prop) : BDD = b.or(that._b)

  override def toString() : String =
    this match {
      case True() => "True"
      case False() => "False"
      case Var(_,n) => n
      case Neg(p) => "~" + p.toString()
      case And(p1, p2) => "(" + p1.toString() + " & " + p2.toString() + ")"
      case Or(p1, p2)  => "(" + p1.toString() + " | " + p2.toString() + ")"
    }

  def isSAT() : Boolean = b.isZero() == false
}

case class True() extends Prop(Config.factory.one())
case class False() extends Prop(Config.factory.zero())
case class Var(index: Int, name: String) extends Prop(Config.factory.ithVar(index))
case class Neg(p : Prop) extends Prop(p.negate)
case class And(p1: Prop, p2: Prop) extends Prop(p1.conj(p2))
case class Or(p1: Prop, p2: Prop) extends Prop(p1.disj(p2))

