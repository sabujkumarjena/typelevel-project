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
import com.allevite.jobsboard.modules.*
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
object Application extends IOApp.Simple {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  override def run: IO[Unit] = {
    ConfigSource.default.loadF[IO, AppConfig].flatMap {
      case AppConfig(postgresConfig, emberConfig, securityConfig) =>
        val appResource = for {
          xa      <- Database.makePostgresResource[IO](postgresConfig)
          core    <- Core[IO](xa)(securityConfig)
          httpApi <- HttpApi[IO](core)
          server <-
            EmberServerBuilder
              .default[IO]
              .withHost(emberConfig.host)
              .withPort(emberConfig.port)
              .withHttpApp(httpApi.endpoints.orNotFound)
              .build
        } yield server
        appResource.use(_ => IO.println("Server ready") >> IO.never)
    }
  }

}
