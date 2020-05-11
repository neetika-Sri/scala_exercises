package shapelessTest
import shapeless._

object LensesDemo {
  case class Address(street: String, city: String, postcode: String)
  case class Person(name:String, age: Int, address: Address)
  val nameLense = lens[Person] >> Symbol("name")
  val ageLens = lens[Person] >> Symbol("age")
  val addressLense = lens[Person] >> Symbol("address")
  val streetLense = lens[Person] >> Symbol("address") >> Symbol("street")
  val cityLense = lens[Person] >> Symbol("address") >> Symbol("city")
  val postCodeLense = lens[Person] >> Symbol("address") >> Symbol("postcode")
}
