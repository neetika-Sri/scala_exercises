package doobieTest
import doobie.implicits._
import doobieTest.Model.{Code, Country, CountryInfo}
import doobieUtils.CountryTable._
import shapeless._
import shapeless.record.Record
object multiColumnQueries {

  val selectUniqueMultiColumns: (String, Int, Option[Double]) =
   // transactorBlock(sql"select name, population, gnp from Country where code='ESP'".query[(String,Int,Option[Double])
   // ].unique)

  transactorBlock {
    sql"select name, population, gnp from Country where code='ESP'"
      .query[(String,Int,Option[Double])]
      .unique
  }.unsafeRunSync()

  type CountryHListType = String :: Int :: Option[Double] :: HNil

  val selectUniqueHList : CountryHListType =
    transactorBlock {
      sql"select name, population, gnp from Country where code='FRA'"
        .query[CountryHListType]
        .unique
    }.unsafeRunSync()

  type Rec = Record.`'name -> String, 'pop -> Int, 'gnp -> Option[Double]`.T

  val selectUniqueRecord: Rec =
    transactorBlock {
      sql"select name, population, gnp from Country where code='USA'"
        .query[Rec]
        .unique
    }.unsafeRunSync()

  val selectUniqueCountry: Country =
    transactorBlock {
      sql"select code,name, population, gnp from Country where name='United Kingdom'"
        .query[Country]
        .unique
    }.unsafeRunSync()

  val selectUniqueCountryCaseTuple: (Code, CountryInfo) =
    transactorBlock {
      sql"select code,name, population, gnp from Country where code='ESP'"
        .query[(Code, CountryInfo)]
        .unique
    }.unsafeRunSync()

  val selectCountriesMap : Map[Code, CountryInfo] =
    transactorBlock {
      sql"select code,name, population, gnp from Country"
        .query[(Code, CountryInfo)]
        .to[List]
    }.unsafeRunSync().toMap

}
