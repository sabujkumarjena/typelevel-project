package com.allevite.jobsboard.modules

import com.allevite.jobsboard.core.{Jobs, LiveJobs}
import cats.effect.*
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*

final class Core[F[_]] private (val jobs: Jobs[F])

// postgres -> jobs -> core  -> httpApi -> app
object Core {
  def postgresResource[F[_]: Async]: Resource[F, HikariTransactor[F]] = for {
    ec <- ExecutionContexts.fixedThreadPool(32)
    xa <- HikariTransactor.newHikariTransactor[F](
      "org.postgresql.Driver",
      "jdbc:postgresql:board", // TODO move to config
      "docker",
      "docker",
      ec
    )
  } yield xa
  def apply[F[_]: Async]: Resource[F, Core[F]] =
    postgresResource[F]
      .evalMap(postgres => LiveJobs[F](postgres))
      .map(jobs => new Core(jobs))
}
