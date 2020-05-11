import cats.instances.map
import org.scalatest.{FunSuite, Matchers}
import shapeless.HNil
import shapeless.PolyDefns.->
import shapeless._
import shapelessTest.CovariantHelper.{APAP, Apple, FFFF, Pear, apap}
import shapelessTest.{ArityDemo, HMapDemo, Polymorphic, SealedDemo, addSize, addSize2, choose, option, sizeM}

import scala.util.Try
class ShaplessTest extends FunSuite with Matchers {
  test("Polymorphic Function Values") {
    choose(Seq(1, 2, 3)) should be(Some(1))
    choose(Seq('a', 'b', 'c')) should be(Some('a'))
    val p = new Polymorphic()
    p.pairApply(choose) should be((Some(1), Some('a')))
    (List(Seq(1, 3, 5), Seq(2, 4, 6)) map choose) should be(List(
      Some(1)
      ,
      Some(2)
    ))
    shapelessTest.size(23) should be(1)
    shapelessTest.size("foo") should be(3)
    shapelessTest.size((23, "foo")) should be(4)
    shapelessTest.size(((23, "foo"), 13)) should be(5)

  }
  test("Heterogeneous List") {

    /*val sets = Set(1) :: Set("foo") :: HNil

    val opts = sets map chooseHL

    opts should be(
      Some(1)
        ::
        Some("foo")
        :: HNil)*/

    import shapeless.poly.identity
    val l = (23 :: "foo" :: HNil) :: HNil :: (true :: HNil) :: HNil

    l flatMap identity should be(23 :: "foo" :: true :: HNil)
    val l2 = 23 :: "foo" :: (13, "wibble") :: HNil

    l2.foldLeft(0)(addSize) should be(
      11
    )
    import syntax.zipper._
    val l3 = 1 :: "foo" :: 3.0 :: HNil
    l3.toZipper.right.put(("wibble",45)).reify should be (1 ::  ("wibble",45) :: 3.0 :: HNil)
    l3.toZipper.right.delete.reify should be(
      1
        ::
        3.0
        :: HNil)

    import scala.reflect.runtime.universe._

    implicitly[TypeTag[APAP]].tpe.typeConstructor <:< typeOf[FFFF] should be (true)
    apap.isInstanceOf[FFFF] should be (true)
    apap.unify.isInstanceOf[APAP] should be (true)
   apap.toList should be (List(Apple(), Pear(), Apple(), Pear()))

    import syntax.typeable._
    val ffff: FFFF = apap.unify
    val precise: Option[APAP] = ffff.cast[APAP]
    precise should be (Some(Apple() :: Pear() :: Apple() :: Pear() :: HNil))

  }
  test("Tuple Test"){
    import syntax.std.tuple._
    import shapeless.poly._

    def drop(res0: Tuple1[Boolean]) =
      (23, "foo", true).drop(2) should be(res0)

    def map(res0: (Option[Int], Option[String], Option[Boolean])) = {
      val l = (23, "foo", true) map option
      l should be(res0)
    }
    import syntax.std.tuple._
    (23, "foo", true).head should be (23)
    (23, "foo", true).tail should be (("foo", true))
    drop(Tuple1(true))
    (23, "foo", true).take(2) should be(
      (23, "foo")
    )
    (23, "foo", true).split(2) should be(
      Tuple2(Tuple2(23, "foo"),Tuple1(true))
    )
    val l = 23 +: ("foo", true)
    l should be(
      (23, "foo", true)
    )
    val l2 = (23, "foo") :+ true
    l2 should be(
      (23, "foo", true)
    )
    val l3 = (23, "foo") ++ (true, 2.0)
    l3 should be(
      (23, "foo" , true, 2.0)
    )
    map(Some(23), Some("foo"), Some(true))
    val l5 = ((23, "foo"), (), (true, 2.0)) flatMap identity
    l5 should be(
      (23, "foo", true, 2.0)
    )
    (23, "foo",(13,"wibble")).foldLeft(0) (addSize2) should be (11)
    (23,"foo",true).productElements should be ((23 :: "foo" :: true :: HNil))
    (23, "foo", true).toList should be(
      List(23, "foo", true)
    )
    import syntax.zipper._
    val l6 = (23, ("foo", true), 2.0 ).toZipper.right.down.put("bar").root.reify
    l6 should be (23,("bar", true), 2.0)

  }
  test("Arity Test"){
    val a = new ArityDemo()
    a.applyProduct((1,2))((_:Int) + (_:Int)) should be (3)
    a.applyProduct((1,2, 3))((_:Int) * (_:Int) * (_:Int)) should be (6)
  }
  test("HMap test") {
    import shapeless._
    import HMapDemo._
    HMapDemo.hm.get(23) should be (Some("foo"))
    HMapDemo.hm.get("bar") should be (Some(13))
/*    implicit val intToStringMapper[Int :: String :: HNil] = new BiMapIS[Int, String]
    implicit val StringToInt = new BiMapIS[String,Int]*/
   /* def mapAsPolyFValue(res0: String :: Int :: HNil) = {
      val l = 23 :: "bar" :: HNil
      val m = l map hm
      m should be(res0)
    }*/
   // mapAsPolyFValue("foo" :: 13 :: HNil)
  }
  test("Singleton type literals"){
    import shapeless.syntax.std.tuple._
    val hlist = 23:: "foo" :: true :: HNil
    hlist(1) should be ("foo")

    val tuple = (23, "foo", true)
    tuple(1) should be ("foo")

    import shapeless._, syntax.singleton._
    def narrow1(res0: Witness) =
      res0.value == 23 should be(true)

    /**
     */
    def narrow2(res0: Witness) =
      res0.value == "foo" should be(true)

    narrow1(23)
    narrow2(Witness("foo"))
    import shapelessTest.SingletonLiteralsDemo._
    select(true)(23) should be (23)
    select(false)("foo") should be ("foo")
  }
  test("Coproduct"){
    type ISB = Int :+: String :+: Boolean :+: CNil
    val isb = Coproduct[ISB]("foo")

    isb.select[Int] should be (None)
    isb.select[String] should be (Some("foo"))
    import shapelessTest.sizeM._
    def mapping(res0: Option[(String, Int)]) = {
      val m = isb map sizeM

      m.select[(String, Int)] should be(res0)
    }
    mapping(Some("foo",3))

   // import union._, syntax.singleton._
   // type U = Union.`'i -> Int, 's -> String, 'b -> Boolean`.type

   // val u = Coproduct[U](Symbol("s")-> "foo")
   // u.get(Symbol("i")) should be(
   // )

  }
  test("Generics"){
    import shapeless.record._
    import shapelessTest.SealedDemo._
    val l = SealedDemo.foogen.to(SealedDemo.foo)
    l should be (23 :: "foo" :: true :: HNil)
    val r = 13 :: l.tail
    val newFoo = SealedDemo.foogen.from(r)
    newFoo.i should be (13)
    val tree: Tree[Int] = Node(Leaf(1), Node(Leaf(2), Leaf(3)))

    everywhere(inc)(tree) should be (Node(Leaf(2), Node(Leaf(3), Leaf(4))))
    case class Book(author: String, title: String, id: Int, price: Double)
    case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)


    /* val bookGen = LabelledGeneric[Book]
    val tapl = Book("Benjamin Pierce", "Types and Programming Language", 262162091, 44.11)
    val rec = bookGen.to(tapl)
    rec(Symbol("price"))  should be ()*/
    val bookGen = LabelledGeneric[Book]
    val tapl = Book("Benjamin Pierce", "Types and Programming Language", 262162091, 44.11)
    val rec = bookGen.to(tapl)
    val updatedBook = bookGen.from(rec.updateWith(Symbol("price"))(_ + 2.0))

    updatedBook.price should be(46.11)
    import syntax.singleton._
    val bookExtGen = LabelledGeneric[ExtendedBook]
    val extendedBook = bookExtGen.from(rec + (Symbol("inPrint") ->> true))
    extendedBook.inPrint should be (true)

  }
  test("Lenses test"){
    import shapelessTest.LensesDemo._
    import shapeless._
    val person = Person("Joe Grey", 37, Address("Southover Street", "Brington", "BN2 9UA"))
    ageLens.get(person) should be (37)
    val updatedPerson = ageLens.set(person)(38)
    updatedPerson.age should be (38)
    val updatedPerson2 = ageLens.modify(person)(_ + 1)
    updatedPerson2.age should be(38)
    streetLense.get(person) should be ("Southover Street")
    val updatedPerson3 = streetLense.set(person)("Montpelier Road")
    updatedPerson3.address.street should be ("Montpelier Road")
  }
  test("Auto type class derivations"){
    import shapelessTest.autoTypleClass._
    case class Foo(i:Int, s:String)
    case class Bar(b:Boolean, s:String, d:Double)
    import MonoidSyntax._

    val fooCombined = Foo(13, "foo" ) |+| Foo(23, "bar")
    fooCombined should be (Foo(36, "foobar"))

    val barCombined = Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
    barCombined should be (Bar(true, "foobar", 4.0))
  }

  test("Lazy test"){
    import shapelessTest.lazyDemo._
    val l : List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    show(l) should be ("Cons(1,Cons(2,Cons(3,Nil)))")
  }
  test("type safe cast test "){
    import shapeless.syntax.typeable._

    val l: Any = List(Vector("foo","bar","baz"), Vector("wibble"))
    l.cast[List[Vector[String]]] should be(
      Some(List(Vector("foo","bar","baz"),Vector("wibble")))
    )
    l.cast[List[Vector[Int]]] should be(
      None
    )
    l.cast[List[List[String]]] should be(
      None
    )
    val `List[String]` = TypeCase[List[String]]
    val `List[Int]` = TypeCase[List[Int]]
    val l2 = List(1,2,3)

    val result = (l2:Any) match {
      case `List[String]`(List(s,_*))=>s.length
      case `List[Int]`(List(i,_*)) => i + 1
    }
    result should be (2)
  }
  test("type checking"){
    import shapeless.test.illTyped

    val matchedType = Try { assertTypeError("illTyped { \"val a: Int = 1\"}")}.isSuccess
    matchedType should be (true)

    val mismatchedType = Try { assertTypeError("illTyped { \"val a: String = 1\"}")}.isSuccess
    mismatchedType should be (false)
  }

}
