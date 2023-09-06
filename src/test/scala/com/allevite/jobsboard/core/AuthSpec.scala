package com.allevite.jobsboard.core

//import cats.*
import cats.data.OptionT
import cats.effect.*
import com.allevite.jobsboard.config.SecurityConfig
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

//  val mockedAuthenticator: Authenticator[IO] = {
//    // key for hashing
//    val key = HMACSHA256.unsafeGenerateKey
//    // identity store to retrieve users
//    val idStore: IdentityStore[IO, String, User] = (email: String) =>
//      if (email == sabujEmail) OptionT.pure(Sabuj)
//      else if (email == deepakEmail) OptionT.pure(Deepak)
//      else OptionT.none[IO, User]
//    // jwt authenticator
//    JWTAuthenticator.unbacked.inBearerToken(
//      // expiry of tokens, max idle, idStore, key
//      1.day,   // expiration of tokens
//      None,    // max idle time (optional)
//      idStore, // identity store
//      key      // hash key
//    )
//  }
//
  "Auth 'algebra'" - {
    "login should return None if the user doesn't exist" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers)
        maybeToken <- auth.login("user@allevite.com", "password")
      } yield maybeToken
      program.asserting(_ shouldBe None)
    }
    "login should return None if the user exists but the password is wrong" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers)
        maybeToken <- auth.login("sabuj@allevite.com", "wrongpassword")
      } yield maybeToken
      program.asserting(_ shouldBe None)
    }

    "login should return a token if the user exists and password is right" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers)
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
        auth <- LiveAuth[IO](mockedUsers)
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
        auth <- LiveAuth[IO](mockedUsers)
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
        auth   <- LiveAuth[IO](mockedUsers)
        result <- auth.changePassword("allice@allevite.com", NewPasswordInfo("oldpw", "newpw"))
      } yield result
      program.asserting(_ shouldBe Right(None))
    }

    "changePassword should return Left with an error if the user exist and password is incorrect " in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers)
        result <- auth.changePassword(sabujEmail, NewPasswordInfo("oldpw", "newpw"))
      } yield result
      program.asserting(_ shouldBe Left("Invalid password"))
    }

    "changePassword should correctly change password if all details are correct " in {
      val program = for {
        auth   <- LiveAuth[IO](mockedUsers)
        result <- auth.changePassword(sabujEmail, NewPasswordInfo("allevite", "scalarocks"))
        isNicePassword <- result match {
          case Right(Some(user)) =>
            BCrypt.checkpwBool[IO]("scalarocks", PasswordHash[BCrypt](user.hashedPassword))

          case _ => IO.pure(false)
        }
      } yield isNicePassword
      program.asserting(_ shouldBe true)
    }

  }

}
