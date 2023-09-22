package com.allevite.jobsboard.modules

import com.allevite.jobsboard.core.{Auth, Emails, Jobs, LiveAuth, LiveEmails, LiveJobs, LiveStripe, LiveTokens, LiveUsers, Stripe, Tokens, Users}
import cats.effect.*
import cats.implicits.*
import com.allevite.jobsboard.config.{EmailServiceConfig, SecurityConfig, StripeConfig, TokenConfig}
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger

final class Core[F[_]] private (val jobs: Jobs[F], val users: Users[F], val tokens: Tokens[F], val emails: Emails[F], val auth: Auth[F], val stripe: Stripe[F])

// postgres -> jobs -> core  -> httpApi -> app
object Core {

  def apply[F[_]: Async: Logger](
      xa: Transactor[F],
      tokenConfig: TokenConfig,
      emailServiceConfig: EmailServiceConfig,
      stripeConfig: StripeConfig
  ): Resource[F, Core[F]] = {
    val coreF = for {
      jobs  <- LiveJobs[F](xa)
      users <- LiveUsers[F](xa)
      tokens <- LiveTokens[F](users)(xa, tokenConfig)
      emails <- LiveEmails[F](emailServiceConfig )
      auth  <- LiveAuth[F](users, tokens, emails)
      stripe <- LiveStripe[F](stripeConfig)
    } yield new Core(jobs, users, tokens, emails, auth, stripe)
    Resource.eval(coreF)
  }

}
