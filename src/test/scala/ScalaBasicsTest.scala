import org.scalatest.{FunSuite, Matchers}

class ScalaBasicsTest extends FunSuite with Matchers{
	test("Map test") {
		val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "MI" -> "Michigan")

		val mapValues = myMap.values
		mapValues.size should be(3)
		mapValues.head should be("Michigan") //Failed presumption: The order in maps is not guaranteed

		val lastElement = mapValues.last
		lastElement should be("Wisconsin")
	}
	test("Pattern Matching") {
		val foodItem = "porridge"

		def goldilocks(expr: (String, String)) = expr match {
			case (`foodItem`, _) => "eating"
			case ("chair", "Mama") => "sitting"
			case ("bed", "Baby") => "sleeping"
			case _ => "what?"
		}

		goldilocks(("porridge", "Papa")) should be(
			"eating"
		)
		goldilocks(("chair", "Mama")) should be(
			"sitting"
		)
		goldilocks(("porridge", "Cousin")) should be(
			"eating"
		)
		goldilocks(("beer", "Cousin")) should be(
			"what?"
		)
	}
	test("Case Class") {
		case class Person(first: String, last: String)

		val p1 = new Person("Fred", "Jones")
		val p2 = new Person("Shaggy", "Rogers")
		val p3 = new Person("Fred", "Jones")

		(p1 == p2) should be(
			false
		)
		(p1 == p3) should be(
			true
		)

		(p1 eq p2) should be(
			false
		)
		(p1 eq p3) should be(
			false
		)
		case class Dog(name: String, breed: String)
		val d1 = Dog("Scooby", "Doberman")
		d1.toString should be(
			"Dog(Scooby,Doberman)"
		)
	}
	test("Partial Function") {
		val doubleEvens: PartialFunction[Int, Int] = {
			case x if (x % 2) == 0 => x * 2
		}
		val tripleOdds: PartialFunction[Int, Int] = {
			case x if (x % 2) != 0 => x * 3
		}

		val printEven: PartialFunction[Int, String] = {
			case x if (x % 2) == 0 => "Even"
		}
		val printOdd: PartialFunction[Int, String] = {
			case x if (x % 2) != 0 => "Odd"
		}

		val whatToDo = doubleEvens orElse tripleOdds andThen (printEven orElse printOdd)

		whatToDo(3) should be(
			"Odd"
		)
		whatToDo(4) should be(
			"Even"
		)
	}
	test("Implicits"){
		import java.math.BigInteger
		implicit def Int2BigIntegerConvert(value: Int): BigInteger =
			new BigInteger(value.toString)

		def add(a: BigInteger, b: BigInteger) = a.add(b)

		add(Int2BigIntegerConvert(3), Int2BigIntegerConvert(6)) == Int2BigIntegerConvert(9) should be(

			true
		)

		add(3, 6) == 9 should be(
			false
		)
		add(3, 6) == Int2BigIntegerConvert(9) should be(
			true
		)

		add(3, 6) == (9: BigInteger) should be(
			true
		)
		add(3, 6).intValue == 9 should be(
			true
		)
	}
	test("For loop"){
		val xValues = 1 to 4
		val yValues = 1 to 2
		val coordinates = for {
			x <- xValues
			y <- yValues
		} yield (x, y)
		coordinates(4) should be((
			3
			,
			1
		))
	}
	test("HashString"){
		import scala.language.postfixOps
		val g: Int = 31
		(g toHexString) should be(
			"1f"
		)
	}
	test("Iterables"){
		val xs = List("Manny", "Moe", "Jack")
		val ys = List("Manny", "Moe", "Jack")
		xs.iterator.sameElements(ys) should be(
			true
		)

		val xt = List("Manny", "Moe", "Jack")
		val yt = List("Manny", "Jack", "Moe")
		xt.iterator.sameElements(yt) should be(
			false
		)

		val xs1 = Set(3, 2, 1, 4, 5, 6, 7)
		val ys1 = Set(7, 2, 1, 4, 5, 6, 3)
		xs1.iterator.sameElements(ys1) should be(
			true
		)

		val xt1 = Set(1, 2, 3)
		val yt1 = Set(3, 2, 1)
		xt1.iterator.sameElements(yt1) should be(
			false
		)
	}

	test("Traversable"){
		val list = List(4, 6, 7, 8, 9, 13, 14)
		val partialFunction1: PartialFunction[Int, Int] = {
			case x: Int if x % 2 == 0 => x * 3
		}
		val partialFunction2: PartialFunction[Int, Int] = {
			case y: Int if y % 2 != 0 => y * 4
		}
		val result = list.collect(partialFunction1 orElse partialFunction2)
		result should be(
			List(12,18,28,24,36,52,42)
		)
	}
	test("Repeated params"){
		def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
			"%d %ss can give you %s".format(x, y, z.mkString(", "))
		}
		repeatedParameterMethod(3, "egg", List("a delicious sandwich", "protein", "high cholesterol")) should be(

			"3 eggs can give you List(a delicious sandwich, protein, high cholesterol)"
		)
	}
	test("Class test"){
		print(classOf[String].getCanonicalName)
		print(classOf[String].getSimpleName)
		classOf[String].getCanonicalName should be(
			"java.lang.String"
		)
		classOf[String].getSimpleName should be(
			"String"
		)
	}
	test("Type Variance"){
		class MyContainer[-A](a: A)(implicit manifest: scala.reflect.Manifest[A]) { //Can't receive a val because it would be in a covariant position
		def contents = manifest.runtimeClass.getSimpleName
	}
		class Fruit
		class Citrus extends Fruit
		class Orange extends Citrus
		class Tangelo extends Citrus
		class Banana extends Fruit
		class Apple extends Fruit

		val citrusBasket: MyContainer[Citrus] = new MyContainer[Citrus](new Orange)
		citrusBasket.contents should be(
			"Citrus$1"
		)
		val orangeBasket: MyContainer[Orange] =
			new MyContainer[Citrus](new Tangelo)
		orangeBasket.contents should be(
			"Citrus$1"
		)
		val tangeloBasket: MyContainer[Tangelo] =
			new MyContainer[Citrus](new Orange)
		tangeloBasket.contents should be(
			"Citrus$1"
		)
		val bananaBasket: MyContainer[Banana] = new MyContainer[Fruit](new Apple)
		bananaBasket.contents should be(
			"Fruit$1"
		)
	}
	test("Enumerations"){
		object Planets extends Enumeration {
			val Mercury = Value
			val Venus = Value
			val Earth = Value
			val Mars = Value
			val Jupiter = Value
			val Saturn = Value
			val Uranus = Value
			val Neptune = Value
			val Pluto = Value
		}

		Planets.Mercury.id should be(
			0
		)
		Planets.Venus.id should be(
			1
		)

		Planets.Mercury.toString should be(
			"Mercury"
		) //How does it get the name? by Reflection.
		Planets.Venus.toString should be(
			"Venus"
		)

		(Planets.Earth == Planets.Earth) should be(
			true
		)
		(Planets.Neptune == Planets.Jupiter) should be(
			false
		)
	}
}
