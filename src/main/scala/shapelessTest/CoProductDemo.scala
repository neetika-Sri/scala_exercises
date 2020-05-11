package shapelessTest

import shapeless.Poly1

object CoProductDemo {

}
object sizeM extends Poly1 {
  implicit def caseInt = at[Int](i => (i,i))
  implicit def caseString = at[String]( s => (s, s.length))
  implicit def caseBoolean = at[Boolean](b =>(b, 1))
}
