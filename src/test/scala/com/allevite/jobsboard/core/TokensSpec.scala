package com.allevite.jobsboard.core

import cats.*
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.allevite.jobsboard.config.TokenConfig
import com.allevite.jobsboard.fixures.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.domain.user.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*
class TokensSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DoobieSpec
    with UserFixture {
  override val initScript: String = "sql/recoverytokens.sql"
  given logger: Logger[IO]        = Slf4jLogger.getLogger[IO]

  "Tokens 'algebra'" - {
    "should not create a new token for a non-existing user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken("somebody@someemail.com")
        } yield token
        program.asserting(_ shouldBe None)
      }
    }

    "should create a new token for an existing user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken(sabujEmail)
        } yield token
        program.asserting(_ shouldBe defined)
      }
    }

    "should not validate expired tokens" in {
      transactor.use { xa =>
        val program = for {
          tokens     <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(100L))
          maybeToken <- tokens.getToken(sabujEmail)
          _          <- IO.sleep(500.millis)
          isTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken(sabujEmail, token)
            case None        => IO.pure(false)
          }
        } yield isTokenValid
        program.asserting(_ shouldBe false)
      }
    }

    "should validate expired tokens that have not expired" in {
      transactor.use { xa =>
        val program = for {
          tokens     <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(1000000L))
          maybeToken <- tokens.getToken(sabujEmail)
          isTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken(sabujEmail, token)
            case None        => IO.pure(false)
          }
        } yield isTokenValid
        program.asserting(_ shouldBe true)
      }
    }

    "should only validate tokens for the user that generated them" in {
      transactor.use { xa =>
        val program = for {
          tokens     <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(1000000L))
          maybeToken <- tokens.getToken(sabujEmail)
          isSabujTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken(sabujEmail, token)
            case None        => IO.pure(false)
          }
          isOtherTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken("someoneelse@gmail.com", token)
            case None        => IO.pure(false)
          }
        } yield (isSabujTokenValid, isOtherTokenValid)
        program.asserting { case (isSabujTokenValid, isOtherTokenValid) =>
          isOtherTokenValid shouldBe false
          isSabujTokenValid shouldBe true
        }
      }
    }

  }

}
