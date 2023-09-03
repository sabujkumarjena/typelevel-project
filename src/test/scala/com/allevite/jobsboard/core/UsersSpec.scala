package com.allevite.jobsboard.core

import cats.*
import cats.effect.*
import cats.implicits.*
import doobie.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.allevite.jobsboard.fixures.UsersFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.allevite.jobsboard.domain.user.*
import org.postgresql.util.PSQLException
class UsersSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Inside
    with DoobieSpec
    with UsersFixture {
  override val initScript: String = "sql/users.sql"
  given logger: Logger[IO]        = Slf4jLogger.getLogger[IO]
  "Users 'algebra'" - {
    "should retrieve a user by email" in {
      transactor.use { xa =>
        val program = for {
          users     <- LiveUsers[IO](xa)
          retrieved <- users.find("deepak@allevite.com")
        } yield retrieved
        program.asserting(_ shouldBe Some(Deepak))
      }
    }

    "should return None if the email doesn't exist" in {
      transactor.use { xa =>
        val program = for {
          users     <- LiveUsers[IO](xa)
          retrieved <- users.find("notfound@allevite.com")
        } yield retrieved
        program.asserting(_ shouldBe None)
      }
    }

    "should create a new user" in {
      transactor.use { xa =>
        val program = for {
          users  <- LiveUsers[IO](xa)
          userId <- users.create(NewUser)
          maybeUser <- sql"SELECT * FROM users WHERE email = ${NewUser.email}"
            .query[User]
            .option
            .transact(xa)
        } yield (userId, maybeUser)

        program.asserting { case (userId, maybeUser) =>
          userId shouldBe NewUser.email
          maybeUser shouldBe Some(NewUser)
        }

      }
    }

    "should fail creating a new user if email already exists " in {
      transactor.use { xa =>
        val program = for {
          users  <- LiveUsers[IO](xa)
          userId <- users.create(Sabuj).attempt // IO[Either[Throwable, String]]
        } yield userId
        program.asserting { outcome =>
          inside(outcome) {
            case Left(e)  => e shouldBe a[PSQLException]
            case Right(_) => fail()
          }
        }

      }
    }

    "should return None when updating a user that doesnot exist" in {
      transactor.use { xa =>
        val program = for {
          users     <- LiveUsers[IO](xa)
          maybeUser <- users.update(NewUser)
        } yield maybeUser
        program.asserting(_ shouldBe None)
      }
    }

    "should update an existing user" in {
      transactor.use { xa =>
        val program = for {
          users     <- LiveUsers[IO](xa)
          maybeUser <- users.update(updatedDeepak)
        } yield maybeUser
        program.asserting(_ shouldBe Some(updatedDeepak))
      }
    }

    "should delete a user" in {
      transactor.use { xa =>
        val program = for {
          users  <- LiveUsers[IO](xa)
          result <- users.delete("sabuj@allevite.com")
          maybeUser <- sql"SELECT * FROM users WHERE email = ${Sabuj.email}"
            .query[User]
            .option
            .transact(xa)
        } yield (result, maybeUser)
        program.asserting { case (result, maybeUser) =>
          result shouldBe true
          maybeUser shouldBe None
        }

      }
    }

    "should NOT delete a user that doesn't exist" in {
      transactor.use { xa =>
        val program = for {
          users  <- LiveUsers[IO](xa)
          result <- users.delete("nobody@allevite.com")
        } yield result
        program.asserting(_ shouldBe false)
      }
    }

  }

}
