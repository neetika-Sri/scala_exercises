package doobieTest

import doobie.implicits._
import doobieUtils.CountryTable._

object dataSelection {

  def selectUniqueCountryName(): String = {
    transactorBlock[String](sql"select name from COUNTRY where code = 'ESP'".query[String].unique).unsafeRunSync()
  }

  val mayBeCountryName : Option[String] =
    transactorBlock(sql"select name from COUNTRY where code = 'ITA'".query[String].option).unsafeRunSync()

  val countryNames : List[String] =
    transactorBlock(sql"select name from COUNTRY order by name".query[String].to[List]).unsafeRunSync()

  val threeCountryNames: List[String]=
    transactorBlock(sql"select name from COUNTRY order by name".query[String].stream.take(3).compile.toList).unsafeRunSync()

}
