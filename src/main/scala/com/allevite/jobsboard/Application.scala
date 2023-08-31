package com.allevite.jobsboard

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import cats.effect.*
import cats.*
import com.allevite.jobsboard.http.routes.HealthRoutes
import org.http4s.ember.server.EmberServerBuilder

/*
    1 - add a plain health endpoint to the app
    2 - add minimal configuration
    3 - basic http server layout
 */

object Application extends IOApp.Simple {

  override def run: IO[Unit] = {
    EmberServerBuilder
      .default[IO]
      .withHttpApp(HealthRoutes[IO].routes.orNotFound)
      .build
      .use(_ => IO.println("Server ready") >> IO.never)
  }

}
