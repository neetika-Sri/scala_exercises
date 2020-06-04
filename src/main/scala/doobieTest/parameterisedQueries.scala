package doobieTest

import cats.data.NonEmptyList
import doobie.Fragments
import doobie.implicits._
import doobieTest.Model.{Code, Country, CountryInfo}
import doobieTest.doobieUtils.CountryTable._
import shapeless._
import shapeless.record.Record

object parameterisedQueries {

 def biggerThan(minPop: Int)=
   sql"""
   select code, name, population, gnp
   from country
   where population > $minPop
   order by population asc
   """.query[Country]

  def populationIn(range: Range) =
    sql"""
				 select code, name, population, gnp
				 from country
		     where population > ${range.min} and population < ${range.max}
			   order by population asc
		 """.query[Country]

  def populationIn(range: Range, codes: NonEmptyList[String]) = {
    val q =
      fr"""
        select code, name, population, gnp
        from country
        where population > ${range.min}
        and population < ${range.max}
        and """ ++ Fragments.in(fr"code", codes)
    q.query[Country]
  }

 val getCountryNameLargePops =
   transactorBlock {
     biggerThan(75000000).to[List]
   }.unsafeRunSync()
   .map(_.name)

  val countriesNameMedPopulation =
    transactorBlock {
      populationIn(25000000 to 75000000).to[List]
    }.unsafeRunSync()
    .map(_.name)

  val getCountriesByPopAndCode =
    transactorBlock {
      populationIn(25000000 to 75000000, NonEmptyList.of("ESP","USA","FRA"))
        .to[List]
    }.unsafeRunSync().map(_.name)
}
