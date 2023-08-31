package com.allevite.basics

import cats.effect.{IO, IOApp}
import doobie.implicits.*
import doobie.util.transactor.Transactor

object Doobie extends IOApp.Simple {

  case class Student(id: Int, name: String)
  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",                 // JDBC connector
    "jdbc:postgresql://localhost:5432/demo", // database URL
    "docker",                                // user
    "docker"                                 // pass
  )

  //read
  def findAllStudentNames: IO[List[String]] = {
    val query  = sql"select name from students".query[String]
    val action = query.to[List]
    action.transact(xa)
  }

  //write
  def saveStudents(id: Int, name: String): IO[Int] = {
    val query = sql"insert into students(id, name) values ($id, $name)"
    val action = query.update.run
    action.transact(xa)
  }

  // read as case class with fragment
  def findStudentsByInitial(letter: String): IO[List[Student]] = {
    val selectPart = fr"select id, name"
    val fromPart = fr"from students"
    val wherePart = fr"where left(name, 1) = $letter"
    val statement = selectPart ++ fromPart ++ wherePart
    val action = statement.query[Student].to[List]
    action.transact(xa)
  }

  override def run: IO[Unit] = {
    IO.unit
    //findAllStudentNames.map(println)
   // saveStudents(3, "alice").map(println)
  }

}
