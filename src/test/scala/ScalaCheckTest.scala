import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
class ScalaCheckTest extends Checkers with Matchers with AnyFunSuite {

  test("test property Strings") {
		"A" should be "A"
	}
}
