package com.allevite.jobsboard

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import cats.effect.*
import cats.*
import com.allevite.jobsboard.config.EmberConfig
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
    configSource match {
      case Left(error) => IO.raiseError(ConfigReaderException[EmberConfig](error))
      case Right(config) =>
        EmberServerBuilder
          .default[IO]
          .withHost(config.host) // String, need Host data structure
          .withPort(config.port) // String, need port
          .withHttpApp(HealthRoutes[IO].routes.orNotFound)
          .build
          .use(_ => IO.println("Server ready") >> IO.never)
    }

  }

}
