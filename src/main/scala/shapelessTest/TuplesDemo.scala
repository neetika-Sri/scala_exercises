package shapelessTest

import shapeless.{Id, Poly1, Poly2}
import shapeless.poly._
/*class TuplesDemo {

}*/
object option extends (Id ~> Option){
  def apply[T](t:T) = Option(t)
}

object sizeOf extends Poly1 {
  implicit def caseInt = at[Int](x => 1)
  implicit def caseString = at[String](_.length)
  implicit def CaseTuple[T,U](implicit st: Case.Aux[T,Int],su: Case.Aux[U, Int]) =
    at[(T, U)](t => sizeOf(t._1) + sizeOf(t._2))
}

object addSize2 extends Poly2 {
  implicit def default[T](implicit st: sizeOf.Case.Aux[T, Int]) =
    at[Int, T]((acc, t) => acc + sizeOf(t))
}

