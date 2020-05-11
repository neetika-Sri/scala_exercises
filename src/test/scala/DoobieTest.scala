import doobieTest.Model
import doobieTest.Model.{Code, CountryInfo}
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuite
import shapeless.record._

class DoobieTest extends AnyFunSuite with Matchers {
  import doobieTest._
  test("Connection"){
    val cons = connection.computeConstant
    cons should be (42)
    val consDB = connection.computeConstantDB
    consDB should be (42)

    val multiConsDB = connection.computeMultiCons
    multiConsDB should be ((42, 25))

    val multiConsDBFunc = connection.computeMultiConsFunctor
    multiConsDBFunc should be (67)
  }
  test("Selecting Data"){
    val uniqueName = dataSelection.selectUniqueCountryName()
    uniqueName should be ("Spain")

    dataSelection.mayBeCountryName should be (None)
    dataSelection.countryNames should be (List("France","Germany","Spain","United Kingdom","United States of America"))
    dataSelection.threeCountryNames should be (List("France","Germany","Spain"))
  }

  test("Multi column Queries"){
   val(name, population, gnp) =  multiColumnQueries.selectUniqueMultiColumns
    gnp should be (None)

    multiColumnQueries.selectUniqueHList.head should be ("France")
    val record = multiColumnQueries.selectUniqueRecord
    record(Symbol("pop")) should be (278357000)

    multiColumnQueries.selectUniqueCountry.code should be ("GBR")
    val (code, country) = multiColumnQueries.selectUniqueCountryCaseTuple
    country.name should be ("Spain")
    import doobieUtils._

    val NotFoundCountry = Model.CountryInfo("Not found", 0, None)
    val countriesMap = multiColumnQueries.selectCountriesMap
    countriesMap.getOrElse(Code("DEU"),NotFoundCountry).name should be ("Germany")
    countriesMap.get(Code("ITA")) should be (None)

  }
}
