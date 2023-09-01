package com.allevite.jobsboard

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import cats.effect.*
import cats.*
import cats.syntax.*
import com.allevite.jobsboard.config.*
import com.allevite.jobsboard.config.syntax.*
import com.allevite.jobsboard.http.HttpApi
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
object Application extends IOApp.Simple {
 given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  override def run: IO[Unit] = {
    ConfigSource.default.loadF[IO, EmberConfig].flatMap { config =>
      EmberServerBuilder
        .default[IO]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(HttpApi[IO].endpoints.orNotFound)
        .build
        .use(_ => IO.println("Server ready") >> IO.never)
    }
  }

}
