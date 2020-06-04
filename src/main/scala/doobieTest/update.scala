package doobieTest

import doobie._
import doobie.implicits._
import doobieUtils.PersonTable._
object update {

	case class Person(id: Long, name: String, age: Option[Short])

	def insert1(name: String, age: Option[Int]): Update0 =
		sql"""
				 insert into person(name, age) values($name, $age)"""
  	.update

	def insertOneRow(): Int = {
     transactorBlock {
			 insert1("John", Option(35)).run
		 }.unsafeRunSync()
	}

	def insertMultiRows(): Int= {
		val rows = for {
		 row1 <- insert1("Alice", Option(12)).run
		 row2 <- insert1("Bob", None).run
		 row3 <- insert1("John", Option(17)).run
		}yield row1 + row2 + row3
		transactorBlock(rows).unsafeRunSync()
	}

	def insertMultiRowsUsingFunctor(): Int = {
		val insertedOnePerson = insert1("Alice", Option(12)).run
		val insertedOtherPerson = insert1("Bob", None).run
		import cats.implicits._
		transactorBlock((insertedOnePerson, insertedOtherPerson).mapN(_ + _)).unsafeRunSync()
	}

	def insertMultiRowsUsingTraverse():List[Int] = {
		import cats.implicits._
		val people =
			List(("Alice", Option(12)), ("Bob", None), ("John", Option(17)), ("Mary", Option(16)))

		transactorBlock(people.traverse(item => (insert1 _).tupled(item).run)).unsafeRunSync()
	}
	def insertUpdateAndSearch():(Int, Int, Option[Short]) = {
		val result = for {
		 insertedRows <- insert1("Alice", Option(12)).run
		 updatedRows <- sql"update person set age = 15 where name = 'Alice'".update.run
		 person <- sql"select id, name, age from person where name = 'Alice'".query[Person].unique
		}yield(insertedRows, updatedRows, person)

		val(insertedRows, updatedRows, person) = transactorBlock(result).unsafeRunSync()
		(insertedRows, updatedRows, person.age)
	}

	def retrieveInfo(): Person = {
		def insert2_H2(name: String, age: Option[Int]): ConnectionIO[Person] = {
			for {
				id <- sql"insert into person (name, age) values ($name, $age)".update
					.withUniqueGeneratedKeys[Int]("id")
				p <- sql"select id, name, age from person where id = $id".query[Person].unique
			} yield p

		}
		transactorBlock(insert2_H2("Ramone", Option(42))).unsafeRunSync()
	}

	def batchUpdate(): Int = {
		import cats.implicits._
		type PersonInfo = (String, Option[Short])
		def insertMany(ps: List[PersonInfo]): ConnectionIO[Int] = {
			val sql = "insert into person (name, age) values (?, ?)"
			Update[PersonInfo](sql).updateMany(ps)

		}
		val data = List[PersonInfo](("Frank", Some(12)),("Daddy", None))
		transactorBlock(insertMany(data)).unsafeRunSync()
	}

}
