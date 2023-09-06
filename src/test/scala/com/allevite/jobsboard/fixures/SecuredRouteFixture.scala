package com.allevite.jobsboard.fixures

import cats.effect.*
import cats.implicits.*
import cats.data.*
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import com.allevite.jobsboard.fixures.*
import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.domain.auth.*
import com.allevite.jobsboard.domain.{auth, user}
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.domain.pagination.Pagination
import com.allevite.jobsboard.domain.security.*
import com.allevite.jobsboard.domain.user.*
import org.http4s.HttpRoutes
import org.http4s.headers.Authorization
import org.typelevel.ci.CIStringSyntax
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import tsec.authentication.{IdentityStore, JWTAuthenticator, SecuredRequestHandler}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

import scala.concurrent.duration.*
trait SecuredRouteFixture extends UserFixture {
  val mockedAuthenticator: Authenticator[IO] = {
    // key for hashing
    val key = HMACSHA256.unsafeGenerateKey
    // identity store to retrieve users
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      if (email == sabujEmail) OptionT.pure(Sabuj)
      else if (email == deepakEmail) OptionT.pure(Deepak)
      else OptionT.none[IO, User]
    // jwt authenticator
    JWTAuthenticator.unbacked.inBearerToken(
      // expiry of tokens, max idle, idStore, key
      1.day,   // expiration of tokens
      None,    // max idle time (optional)
      idStore, // identity store
      key      // hash key
    )
  }

  extension (r: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] = r.putHeaders {
      val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)
      // Authorization: Bearer {jwt}
      Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
    }

  given securedHandler: SecuredHandler[IO] = SecuredRequestHandler(mockedAuthenticator)
}
