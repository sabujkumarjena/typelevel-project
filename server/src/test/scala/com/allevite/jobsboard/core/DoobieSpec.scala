package com.allevite.jobsboard.core

import cats.effect.*
import doobie.hikari.HikariTransactor
import org.testcontainers.containers.PostgreSQLContainer
//import cats.effect.kernel.Resource
import doobie.*
import doobie.implicits.*
import doobie.util.*

trait DoobieSpec {
//simulate a database
//docker containers
//test containers

  val initScript: String // to be implemented by whatever test case interacts with the DB

  val postgres: Resource[IO, PostgreSQLContainer[Nothing]] = {
    val acquire = IO {
      val container: PostgreSQLContainer[Nothing] =
        new PostgreSQLContainer("postgres").withInitScript(initScript)
      container.start()
      container
    }
    val release = (container: PostgreSQLContainer[Nothing]) => IO(container.stop())
    Resource.make(acquire)(release)
  }
// set up a Postgres transactor
  val transactor: Resource[IO, Transactor[IO]] = for {
    db <- postgres
    ce <- ExecutionContexts.fixedThreadPool[IO](1)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      db.getJdbcUrl(),
      db.getUsername(),
      db.getPassword(),
      ce
    )
  } yield xa
}
