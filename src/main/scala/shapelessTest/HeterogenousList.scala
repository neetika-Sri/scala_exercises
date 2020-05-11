package shapelessTest

import shapeless.poly._
import shapeless.{HNil, Poly2}
import shapeless._

/*class HeterogenousList {

}*/
object chooseHL extends (Seq ~> Option){

  def apply[T](f: Seq[T]): Option[T] = f.headOption

}

object addSize extends Poly2 {
  implicit def default[T](implicit st:shapelessTest.size.Case.Aux[T, Int]) =
    at[Int, T] {(acc, t) => acc + shapelessTest.size(t)}
}

object CovariantHelper {
  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit

  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil

  val a : Apple = Apple()
  val p : Pear = Pear()
  val apap: APAP = a :: p :: a :: p :: HNil
}

