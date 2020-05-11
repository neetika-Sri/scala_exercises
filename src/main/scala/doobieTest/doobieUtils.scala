package doobieTest

import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.util.ExecutionContexts
import cats.effect._
import cats.implicits._
import doobie._
import doobie.free.connection.ConnectionIO
import doobie.h2.H2Transactor
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobieTest.Model._

object doobieUtils {
  implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

  object CountryTable {

    val createCountryTable: ConnectionIO[Int] =
      sql"""
           Create table if not exists country(
            code character (3) NOT NULL,
            name text NOT NULL,
            population integer NOT NULL,
            gnp numeric (10, 2))
           """.update.run

    def insertCountries(countries: List[Country]):ConnectionIO[Int] =
      Update[Country]("insert into country(code, name, population, gnp) values(?,?,?,?)").updateMany(countries)

    def transactorBlock[A](f: => ConnectionIO[A]): IO[A]={
      val transactor = connection.conn()
      transactor.use((createCountryTable *> insertCountries(countries) *> f).transact[IO])
    }

  }


}
