package com.allevite.jobsboard.core

//import cats.*
import cats.data.OptionT
import cats.effect.*
import com.allevite.jobsboard.config.SecurityConfig
import tsec.authentication.unliftedCookieFromRequest
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash
//import cats.implicits.*
//import doobie.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.allevite.jobsboard.fixures.UserFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.allevite.jobsboard.domain.user.*
import com.allevite.jobsboard.domain.security.*
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256
import com.allevite.jobsboard.domain.auth.*
//import org.postgresql.util.PSQLException

import scala.concurrent.duration.*
class AuthSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with UserFixture {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val mockedConfig = SecurityConfig("secrete", 1.day)
  val mockedTokens: Tokens[IO] = new Tokens[IO] {
    override def getToken(email: String): IO[Option[String]] =
      if (email == sabujEmail) IO.pure("abc123").map(Some(_))
      else IO.pure(None)

    override def checkToken(email: String, token: String): IO[Boolean] = IO.pure(token == "abc123")
  }

  val mockedEmails: Emails[IO] = new Emails[IO] {
    override def sendEmail(to: String, subject: String, content: String): IO[Unit] = IO.unit

    override def sendPasswordRecoveryEmail(to: String, token: String): IO[Unit] = IO.unit
  }

  def probedEmails(users: Ref[IO, Set[String]]): Emails[IO] = new Emails[IO] {
    override def sendEmail(to: String, subject: String, content: String): IO[Unit] =
      users.modify(set => (set + to, ()))

    override def sendPasswordRecoveryEmail(to: String, token: String): IO[Unit] =
      sendEmail(to, "your token", "token")
  }

  "Auth 'algebra'" - {
    "login should return None if the user doesn't exist" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeToken <- auth.login("user@allevite.com", "password")
      } yield maybeToken
      program.asserting(_ shouldBe None)
    }
    "login should return None if the user exists but the password is wrong" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeToken <- auth.login("sabuj@allevite.com", "wrongpassword")
      } yield maybeToken
      program.asserting(_ shouldBe None)
    }

    "login should return a token if the user exists and password is right" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeToken <- auth.login("sabuj@allevite.com", "allevite")
      } yield maybeToken
      program.asserting(_ shouldBe defined)
    }

//    "login should return a token if the user exists and password is right" in {
//      val program = for {
//        auth       <- LiveAuth[IO](mockedUsers)(mockedConfig)
//        maybeToken <- auth.login("sabuj@allevite.com", "allevite")
//      } yield maybeToken
//      program.asserting(_ shouldBe defined)
//    }

    "signing up should not create a user with existing email" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeUser <- auth.signUp(
          NewUserInfo(
            sabujEmail,
            "somePassword",
            Some("sabuj"),
            Some("Jena"),
            Some("Other company")
          )
        )
      } yield maybeUser
      program.asserting(_ shouldBe None)
    }

    "signing up should create a completely new user " in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeUser <- auth.signUp(
          NewUserInfo(
            "bob@allevite.com",
            "somePassword",
            Some("Bob"),
            Some("Jones"),
            Some("Company")
          )
        )
      } yield maybeUser
      program.asserting {
        case Some(user) =>
          user.email shouldBe "bob@allevite.com"
          user.firstName shouldBe Some("Bob")
          user.lastName shouldBe Some("Jones")
          user.company shouldBe Some("Company")
          user.role shouldBe Role.RECRUITER
        case _ => fail()
      }
    }
    "changePassword should return Right(None) if the user doesn't exist" in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.changePassword("allice@allevite.com", NewPasswordInfo("oldpw", "newpw"))
      } yield result
      program.asserting(_ shouldBe Right(None))
    }

    "changePassword should return Left with an error if the user exist and password is incorrect " in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.changePassword(sabujEmail, NewPasswordInfo("oldpw", "newpw"))
      } yield result
      program.asserting(_ shouldBe Left("Invalid password"))
    }

    "changePassword should correctly change password if all details are correct " in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.changePassword(sabujEmail, NewPasswordInfo("allevite", "scalarocks"))
        isNicePassword <- result match {
          case Right(Some(user)) =>
            BCrypt.checkpwBool[IO]("scalarocks", PasswordHash[BCrypt](user.hashedPassword))

          case _ => IO.pure(false)
        }
      } yield isNicePassword
      program.asserting(_ shouldBe true)
    }

    "recoverPassword should fail for  user that doesn't exist, even if the token is correct " in {
      val program = for {
        auth    <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result1 <- auth.recoverPasswordFromToken("someone@gmail.com", "abc123", "igotya")
        result2 <- auth.recoverPasswordFromToken("someone@gmail.com", "wrongtoken", "igotya")
      } yield (result1, result2)
      program.asserting(_ shouldBe (false, false))
    }

    "recoverPassword should fail for a user that exists, but the token is wrong " in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.recoverPasswordFromToken(sabujEmail, "wrogtoken", "igotya")
      } yield result
      program.asserting(_ shouldBe false)
    }

    "recoverPassword should succeed for a correct combination of user/token " in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.recoverPasswordFromToken(sabujEmail, "abc123", "igotya")
      } yield result
      program.asserting(_ shouldBe true)
    }

    "sending recovery password should fail for a user that doesn't exist " in {
      val program = for {
        set                 <- Ref.of[IO, Set[String]](Set())
        emails              <- IO(probedEmails(set))
        auth                <- LiveAuth[IO](mockedUsers, mockedTokens, emails)
        result              <- auth.sendPasswordRecoveryToken("someone@gmail.com")
        usersBengSentEmails <- set.get
      } yield usersBengSentEmails
      program.asserting(_ shouldBe empty)
    }

    "sending recovery password should succeed for a user that exists " in {
      val program = for {
        set <- Ref.of[IO, Set[String]](Set())
        emails <- IO(probedEmails(set))
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, emails)
        result <- auth.sendPasswordRecoveryToken(sabujEmail)
        usersBengSentEmails <- set.get
      } yield usersBengSentEmails
      program.asserting(_ should contain(sabujEmail))
    }

  }

}
