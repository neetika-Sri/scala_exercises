import cats.{Apply, Foldable, Functor, Monad}
import cats.kernel.Semigroup
import org.scalatest.{FunSuite, Matchers}
object EitherStyle {
  import cats.implicits._
  def parse(s: String): Either[NumberFormatException, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

  def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
    if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
    else Either.right(1.0 / i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Exception, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}
class CatBasicsTest extends FunSuite with Matchers{
  import cats.implicits._
  test("SemiGroup"){
    Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) should be(
      67
    )
  }
  test("Functors"){
    Functor[Option].map(Option("Hello"))(_.length) should be(
      Some(5)
    )
    Functor[Option].map(None: Option[String])(_.length) should be(
      None
    )
  }
  test("Apply"){
    import cats.implicits._

    val intToString: Int => String = _.toString
    val double: Int => Int = _ * 2
    val addTwo: Int => Int = _ + 2
    val listOpt = Apply[List] compose Apply[Option]
    val plusOne =(x:Int) => x + 1
    listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) should be(List(Some(2),None, Some(4)))

    Apply[Option].ap(Some(intToString))(Some(1)) should be(
      Some("1")
    )
    Apply[Option].ap(Some(double))(Some(1)) should be(
      Some(2)
    )
    Apply[Option].ap(Some(addTwo))(None) should be(
      None
    )
    Apply[Option].ap(None)(Some(1)) should be(
      None
    )
    import cats.implicits._
    val option2 = (Option(1), Option(2))
    val option3 = (option2._1, option2._2, Option.empty[Int])
    val addArity2 = (a: Int, b: Int) => a + b
    val addArity3 = (a: Int, b: Int, c: Int) => a + b + c

    option2 mapN addArity2 should be(
      Some(3)
    )
    option3 mapN addArity3 should be(
      None
    )

    option2 apWith Some(addArity2) should be(
      Some(3)
    )
    option3 apWith Some(addArity3) should be(
      None
    )

    option2.tupled should be(
      Some(1,2)
    )
    option3.tupled should be(
      None
    )
  }
  test("Monad"){
    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be(
      Some("truthy")
    )
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(
      List(1,2,3,4,1,2)
    )
  }
  test("Foldable"){
    Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be(
      List(1,2,3,4,5)
    )
    Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be(
      Some("two")
    )
    import cats.implicits._

    def parseInt(s: String): Option[Int] =
      Either.catchOnly[NumberFormatException](s.toInt).toOption

    Foldable[List].traverse_(List("1", "2", "3"))(parseInt) should be(
      Some(())
    )
    Foldable[List].traverse_(List("a", "b", "c"))(parseInt) should be(
      None
    )
  }
  test("Either") {
    import EitherStyle._

    val result = magic("2") match {
      case Left(_: NumberFormatException) => "Not a number!"
      case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
      case Left(_) => "Unknown error"
      case Right(result) => s"Got reciprocal: ${result}"
    }
    result should be(
      "Got reciprocal: 0.5"
    )
    val right: Either[String, Int] = Right(41)
    right.map(_ + 1) should be(
      Right(42)
    )

    val left: Either[String, Int] = Left("Hello")
    left.map(_ + 1) should be(
      Left("Hello")
    )
    left.leftMap(_.reverse) should be(
      Left("olleH")
    )
  }
  test("Traverse"){
    import cats.implicits._

    List(Option(1), Option(2), Option(3)).traverse(identity) should be(
      Some(List(1,2,3))
    )
    List(Option(1), None, Option(3)).traverse(identity) should be(
      None
    )
    List(Option(1), Option(2), Option(3)).sequence should be(
      Some(List(1,2,3))
    )
    List(Option(1), None, Option(3)).sequence should be(
      None
    )
  }
}
