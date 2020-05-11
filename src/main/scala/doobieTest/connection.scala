package doobieTest

import doobie._
import doobie.implicits._
import cats._
import cats.effect._
import cats.implicits._
import doobie.hikari._
import doobie.util.{ExecutionContexts, transactor}
import cats.effect.IO._
import doobie.h2.H2Transactor

object connection {
  def conn():Resource[IO, H2Transactor[IO]] ={
    val program1 = 42.pure[ConnectionIO]
    implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

    val transactor: Resource[IO, H2Transactor[IO]] = {
      def url  = "jdbc:h2:mem:"
      val user = "sa"
      val pass = ""

      for {
        ec <- ExecutionContexts.fixedThreadPool[IO](1)
        bc <- Blocker[IO]
        xa <- H2Transactor.newH2Transactor[IO](url, user, pass, ec, bc)
      } yield xa
    }
    transactor
  }

  def computeConstant: Int = {
    val transactor = conn()
    transactor.use(42.pure[ConnectionIO].transact[IO]).unsafeRunSync()
  }

  def computeConstantDB:Int = {
    val transactor = conn()
    transactor.use(sql"select 42".query[Int].unique.transact[IO]).unsafeRunSync()
  }

  def computeMultiCons = {
   val transactor = conn()
   val largerProgram = for{
     a <- sql"select 42".query[Int].unique
     b <- sql"select power(5,2)".query[Int].unique
   }yield (a,b)
    transactor.use(largerProgram.transact[IO]).unsafeRunSync()
  }

  def computeMultiConsFunctor = {
    val transactor = conn()
    val oneProgram = sql"select 42".query[Int].unique
    val anotherProgram = sql"select power(5,2)".query[Int].unique
    transactor.use((oneProgram, anotherProgram).mapN(_ + _).transact[IO]).unsafeRunSync()
  }

}

