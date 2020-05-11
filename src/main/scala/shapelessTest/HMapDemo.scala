package shapelessTest

import shapeless.HMap
import shapeless.poly._


object HMapDemo {
  class BiMapIS[K,V]
  implicit val intToString = new BiMapIS[Int, String]
  implicit val StringToInt = new BiMapIS[String,Int]
  val hm = HMap[BiMapIS](23->"foo","bar"->13)

}
