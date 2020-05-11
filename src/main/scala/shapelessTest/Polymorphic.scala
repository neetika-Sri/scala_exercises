package shapelessTest

import shapeless.Poly1
import shapeless.PolyDefns.~>

class Polymorphic {
  def pairApply(f: Seq ~> Option) = (f(Seq(1,2,3)), f(Seq('a','b','c')))

}
object choose extends (Seq ~> Option) {
  def apply[T](s: Seq[T]) = s.headOption
}

object size extends Poly1 {
  implicit def caseInt = at[Int](x=>1)
  implicit def caseString = at[String](_.length)
  implicit def caseTuple[T,U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int])
  = at[(T,U)](t => size(t._1) + size(t._2))

}
