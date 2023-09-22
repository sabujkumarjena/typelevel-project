package com.allevite.jobsboard.modules

import cats.*
import cats.effect.*
import cats.implicits.*
import com.allevite.jobsboard.core.Users
import tsec.authentication.SecuredRequestHandler
//import com.allevite.jobsboard.http.HttpApi
import com.allevite.jobsboard.http.routes.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.typelevel.log4cats.Logger
import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import com.allevite.jobsboard.config.SecurityConfig
import com.allevite.jobsboard.domain.auth.*
import com.allevite.jobsboard.domain.security.*
import com.allevite.jobsboard.domain.user.*
import org.typelevel.log4cats.Logger
import tsec.authentication.{BackingStore, IdentityStore, JWTAuthenticator}
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

import scala.concurrent.duration.*
class HttpApi[F[_]: Async: Logger] private (core: Core[F], authenticator: Authenticator[F]) {
  given requestHandler: SecuredHandler[F] = SecuredRequestHandler(authenticator)
  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F](core.jobs, core.stripe).routes
  private val authRoutes = AuthRoutes[F](core.auth, authenticator).routes

  val endpoints = Router(
    "/api" -> (healthRoutes <+> jobRoutes <+> authRoutes)
  )
}

object HttpApi {

  def createAuthenticator[F[_]:Sync](users: Users[F], securityConfig: SecurityConfig): F[Authenticator[F]] = {
    // 1. Identity store: String => OptionT[F, User]
    // identity store to retrieve users
    val idStore: IdentityStore[F, String, User] = (email: String) => OptionT(users.find(email))

    // 2. backing store for JWT token : BackingStore[F,id, JwtToken]
    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
      new BackingStore[F, SecureRandomId, JwtToken] {
        // mutable map - race condition
        // ref
        override def get(id: SecureRandomId): OptionT[F, JwtToken] =
          OptionT(/*F[JwtToken]*/ ref.get.map(_.get(id)))

        override def put(elem: JwtToken): F[JwtToken] =
          ref.modify(store => (store + (elem.id -> elem), elem))

        override def delete(id: SecureRandomId): F[Unit] = ref.modify(store => (store - id, ()))

        override def update(v: JwtToken): F[JwtToken] = put(v)

      }
    }
    // 3. key for hashing
    val keyF = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8")) // TODO move to config

    for {
      key <- keyF
      tokenStore <- tokenStoreF
      // 4. jwt authenticator

    } yield
       JWTAuthenticator
    .backed.inBearerToken(
      // expiry of tokens, max idle, idStore, key
      expiryDuration = securityConfig.jwtExpiryDuration, // expiration of tokens
      maxIdle = None, // max idle time (optional)
      identityStore = idStore, // identity store
      tokenStore = tokenStore,
      signingKey = key // hash key
    )

  }
  def apply[F[_]: Async: Logger](core: Core[F], securityConfig: SecurityConfig): Resource[F, HttpApi[F]] =
    Resource.eval(createAuthenticator(core.users, securityConfig))
      .map(authenticator => new HttpApi[F](core, authenticator))

}
