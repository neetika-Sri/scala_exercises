package shapelessTest

import shapeless.{Witness, WitnessWith}
import shapeless._, syntax.singleton._

object SingletonLiteralsDemo {
 val (wTrue, wFalse) = (Witness(true), Witness(false))

  type True = wTrue.T
  type False = wFalse.T

  trait Select[B]{type Out}

  implicit val selInt = new Select[True]{type Out = Int}
  implicit val selString = new Select[False]{type Out = String}

  def select(b: WitnessWith[Select])(t: b.instance.Out)=t
 // select(true)(13)

 /* def main(args: Array[String]): Unit ={
    println(select(true)("foo"))
  }*/

}
