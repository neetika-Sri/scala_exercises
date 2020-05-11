package shapelessTest

import shapeless.{Generic, LabelledGeneric}

object SealedDemo {

  import shapeless.record._

  case class Foo(i: Int, s: String, b:Boolean)
  val foogen = Generic[Foo]
  val foo = Foo(23, "foo", true)
  import shapeless.poly._

  sealed trait Tree[T]
  case class Leaf[T](t:T) extends Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  object inc extends -> ((i: Int) => i+1)


}
