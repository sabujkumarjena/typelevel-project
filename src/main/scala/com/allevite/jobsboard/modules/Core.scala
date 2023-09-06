package com.allevite.jobsboard.modules

import com.allevite.jobsboard.core.{Auth, Jobs, LiveAuth, LiveJobs, LiveUsers, Users}
import cats.effect.*
import cats.implicits.*
import com.allevite.jobsboard.config.SecurityConfig
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger

final class Core[F[_]] private (val jobs: Jobs[F], val users: Users[F], val auth: Auth[F])

// postgres -> jobs -> core  -> httpApi -> app
object Core {

  def apply[F[_]: Async: Logger](
      xa: Transactor[F]
  ): Resource[F, Core[F]] = {
    val coreF = for {
      jobs  <- LiveJobs[F](xa)
      users <- LiveUsers[F](xa)
      auth  <- LiveAuth[F](users)
    } yield new Core(jobs, users, auth)
    Resource.eval(coreF)
  }

}
