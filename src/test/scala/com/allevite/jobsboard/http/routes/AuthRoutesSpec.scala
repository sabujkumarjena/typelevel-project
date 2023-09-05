package com.allevite.jobsboard.http.routes

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
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

import scala.concurrent.duration.*
class AuthRoutesSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with UserFixture {
  ////////////////
  // prep
  //////////////////
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

  val mockedAuth: Auth[IO] = new Auth[IO] {
    // TODO make sure only sabuj already exists
    override def login(email: String, password: String): IO[Option[JwtToken]] =
      if (email == sabujEmail && password == sabujPassword)
        mockedAuthenticator.create(sabujEmail).map(Some(_))
      else IO.pure(None)

    override def signUp(newUserInfo: user.NewUserInfo): IO[Option[user.User]] =
      if (newUserInfo.email == deepakEmail) IO.pure(Some(Deepak))
      else IO.pure(None)

    override def changePassword(
        email: String,
        newPasswordInfo: auth.NewPasswordInfo
    ): IO[Either[String, Option[user.User]]] =
      if (email == sabujEmail)
        if (newPasswordInfo.oldPassword == sabujPassword)
          IO.pure(Right(Some(Sabuj)))
        else
          IO.pure(Left("Invalid password"))
      else
        IO.pure(Right(None))

    override def delete(email: String): IO[Boolean] = IO.pure(true)
    override def authenticator: Authenticator[IO]   = mockedAuthenticator
  }

  extension (r: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] = r.putHeaders {
      val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)
      // Authorization: Bearer {jwt}
      Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
    }
  given logger: Logger[IO]       = Slf4jLogger.getLogger[IO]
  val authRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth).routes

  ////////////////////////
  // tests
  ////////////////////
  "AuthRoutes" - {
    "should return a 401 - unauthorized if login fails " in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(sabujEmail, "wrongpassword"))
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 -OK + a JWT if login is sucessful " in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(sabujEmail, sabujPassword))
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Ok
        response.headers.get(ci"Authorization") shouldBe defined
      }
    }

    "should return a 400 -Bad Request if the user to create is already exists " in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/users")
            .withEntity(NewUserSabuj)
        )
      } yield {
        // assertions here
        response.status shouldBe Status.BadRequest
      }
    }

    "should return a 201 - Created if the user to create doesn't exist " in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/users")
            .withEntity(NewUserDeepak)
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Created
      }
    }

    "should return a 200 - Ok if logging out with a valid JWT token " in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
            .withBearerToken(jwtToken)
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Ok
      }
    }

    "should return a 401 - Unauthorized if logging out without a valid JWT token " in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Unauthorized
      }
    }

    // change password - user doesn't exist => 404 Not Found

    "should return a 404 - Unauthorized if changing password for user that doesn't exist " in {
      for {
        jwtToken <- mockedAuthenticator.create(deepakEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withBearerToken(jwtToken)
            .withEntity(NewPasswordInfo(deepakPassword, "newpassword"))
        )
      } yield {
        // assertions here
        response.status shouldBe Status.NotFound
      }
    }
    // change password - invalid old password => 403 Forbidden

    "should return a 403 - Forbidden if old password is incorrect " in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withBearerToken(jwtToken)
            .withEntity(NewPasswordInfo("wrongpassword", "newpassword"))
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Forbidden
      }
    }

    // change password - user JWT is invalid => 401 Unauthorized

    "should return a 401 - Unauthorized if changing password without a JWT " in {
      for {

        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withEntity(NewPasswordInfo(sabujPassword, "newpassword"))
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Unauthorized
      }
    }
    // change password - happy path 200 Ok

    "should return a 200 - OK if changing password of a user with valid JWT token  and password" in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withBearerToken(jwtToken)
            .withEntity(NewPasswordInfo(sabujPassword, "newpassword"))
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Ok
      }
    }

    "should return a 401 - Unauthorized  if a non-admin tries to delete a user" in {
      for {
        jwtToken <- mockedAuthenticator.create(deepakEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/auth/users/sabuj@allevite.com")
            .withBearerToken(jwtToken)
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - OK if an admin tries to delete a user" in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/auth/users/sabuj@allevite.com")
            .withBearerToken(jwtToken)
        )
      } yield {
        // assertions here
        response.status shouldBe Status.Ok
      }
    }

  }
}
