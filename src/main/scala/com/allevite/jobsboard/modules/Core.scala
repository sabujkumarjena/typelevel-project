package com.allevite.jobsboard.modules

import com.allevite.jobsboard.core.{Jobs, LiveJobs}
import cats.effect.*
import cats.implicits.*
import doobie.util.transactor.Transactor

final class Core[F[_]] private (val jobs: Jobs[F])

// postgres -> jobs -> core  -> httpApi -> app
object Core {

  def apply[F[_]: Async](xa: Transactor[F]): Resource[F, Core[F]] =
    Resource
      .eval(LiveJobs[F](xa))
      .map(jobs => new Core(jobs))
}
