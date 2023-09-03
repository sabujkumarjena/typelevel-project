package com.allevite.jobsboard.whiteboard

import cats.effect.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.core.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger

import scala.io.StdIn

object JobsWhiteboard extends IOApp.Simple {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  val postgresResource: Resource[IO, HikariTransactor[IO]] = for {
    ec <- ExecutionContexts.fixedThreadPool(32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:board",
      "docker",
      "docker",
      ec
    )
  } yield xa

  val jobInfo = JobInfo.minimal(
    company = "Allevite",
    title = "Software Engineer",
    description = "best job",
    externalUrl = "allevite.com",
    remote = false,
    location = "Anywhere"
  )
  override def run: IO[Unit] = postgresResource.use { xa =>
    for {
      jobs      <- LiveJobs[IO](xa)
      _         <- IO(println("Ready. Next...")) >> IO(StdIn.readLine())
      id        <- jobs.create("sabuj@allevite,com", jobInfo)
      _         <- IO(println("Next...")) >> IO(StdIn.readLine())
      list      <- jobs.all()
      _         <- IO(println(s"All jobs: $list. Next...")) >> IO(StdIn.readLine())
      _         <- jobs.update(id, jobInfo.copy(title = "Software rockstar"))
      newJob    <- jobs.find(id)
      _         <- IO(println(s"New job: $newJob. Next...")) >> IO(StdIn.readLine())
      _         <- jobs.delete(id)
      listAfter <- jobs.all()
      _ <- IO(println(s"Deleted job. List now: $listAfter. Next...")) >> IO(StdIn.readLine())
    } yield ()
  }
}
