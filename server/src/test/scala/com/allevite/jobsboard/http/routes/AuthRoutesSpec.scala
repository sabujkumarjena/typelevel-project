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
    with UserFixture
    with SecuredRouteFixture {
  ////////////////
  // prep
  //////////////////

  val mockedAuth: Auth[IO] = probedAuth(None)

  def probedAuth(userMap: Option[Ref[IO, Map[String, String]]]): Auth[IO] =
    new Auth[IO] {
      // TODO make sure only sabuj already exists
      override def login(email: String, password: String): IO[Option[User]] =
        if (email == sabujEmail && password == sabujPassword)
          IO(Some(Sabuj))
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

      def sendPasswordRecoveryToken(email: String): IO[Unit] = userMap
        .traverse { userMapRef =>
          userMapRef.modify { userMap =>
            (userMap + (email -> "abc123"), ())
          }
        }
        .map(_ => ())

      def recoverPasswordFromToken(email: String, token: String, newPassword: String): IO[Boolean] =
        userMap
          .traverse { userMapRef =>
            userMapRef.get
              .map { userMap =>
                userMap.get(email).filter(_ == token)
              }                // IO[Option[String]]
              .map(_.nonEmpty) // IO[Boolean]
          }
          .map(_.getOrElse(false))

      def authenticator: Authenticator[IO] = mockedAuthenticator
    }

  given logger: Logger[IO]       = Slf4jLogger.getLogger[IO]
  val authRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth, mockedAuthenticator).routes

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

    "should return a 200 - OK when resetting a password, and an email should be triggered" in {
      for {
        userMapRef <- Ref.of[IO, Map[String, String]](Map())
        auth       <- IO(probedAuth(Some(userMapRef)))
        routes     <- IO(AuthRoutes(auth, mockedAuthenticator).routes)
        response <- routes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/reset")
            .withEntity(ForgotPasswordInfo(sabujEmail))
        )
        userMap <- userMapRef.get
      } yield {
        // assertions here
        response.status shouldBe Status.Ok
        userMap should contain key (sabujEmail)
      }
    }

    "should return a 200 - OK when recovering  a password for a correct user/token combination " in {
      for {
        userMapRef <- Ref.of[IO, Map[String, String]](Map(sabujEmail -> "abc123"))
        auth       <- IO(probedAuth(Some(userMapRef)))
        routes     <- IO(AuthRoutes(auth, mockedAuthenticator).routes)
        response <- routes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/recover")
            .withEntity(RecoverPasswordInfo(sabujEmail, "abc123", "newpassword"))
        )
        userMap <- userMapRef.get
      } yield {
        // assertions here
        response.status shouldBe Status.Ok
      }
    }

    "should return a 403 - Forbidden when recovering  a password for a user with incorrect token " in {
      for {
        userMapRef <- Ref.of[IO, Map[String, String]](Map(sabujEmail -> "abc123"))
        auth <- IO(probedAuth(Some(userMapRef)))
        routes <- IO(AuthRoutes(auth, mockedAuthenticator))
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/recover")
            .withEntity(RecoverPasswordInfo(sabujEmail, "wrong token", "newpassword"))
        )
        userMap <- userMapRef.get
      } yield {
        // assertions here
        response.status shouldBe Status.Forbidden
      }
    }

  }
}
