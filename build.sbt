name := "Leetcode1"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

libraryDependencies += "org.scala-exercises" %% "definitions" % "0.6.0"

libraryDependencies += "org.tpolecat" %% "doobie-core" % "0.9.0"

libraryDependencies += "org.tpolecat" %% "doobie-hikari" % "0.9.0"

libraryDependencies += "org.tpolecat" %% "doobie-h2" % "0.9.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % "test"

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1"

libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
