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
import com.allevite.jobsboard.http.routes.HealthRoutes
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

/*
    1 - add a plain health endpoint to the app
    2 - add minimal configuration
    3 - basic http server layout
 */

object Application extends IOApp.Simple {

  val configSource = ConfigSource.default.load[EmberConfig]
  override def run: IO[Unit] = {
    ConfigSource.default.loadF[IO, EmberConfig].flatMap { config =>
      EmberServerBuilder
        .default[IO]
        .withHost(config.host)
        .withPort(config.port) 
        .withHttpApp(HealthRoutes[IO].routes.orNotFound)
        .build
        .use(_ => IO.println("Server ready") >> IO.never)
    }
  }

}
