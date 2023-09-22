package com.allevite.jobsboard.core

import cats.*
import cats.effect.*
import doobie.*
import doobie.util.*
import doobie.postgres.implicits.*
import doobie.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.allevite.jobsboard.fixures.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.domain.pagination.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
//import com.allevite.jobsboard.Application.logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
class JobsSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DoobieSpec
    with JobFixture {
  override val initScript: String = "sql/jobs.sql"

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Jobs 'algebra'" - {
    "should return no job if the given UUID does not exist" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.find(NotFoundJobUuid)
        } yield retrieved
        program.asserting(_ shouldBe None)
      }
    }

    "should return a job by id" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.find(AwesomeJobUuid)
        } yield retrieved
        program.asserting(_ shouldBe Some(AwesomeJob))
      }
    }

    "should retrieve all jobs" in {
      transactor.use { xa =>
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.all()
        } yield retrieved
        program.asserting(_ shouldBe List(AwesomeJob))
      }
    }

    "should create a new job initially inactive" in {
      transactor.use { xa =>
        val program = for {
          jobs     <- LiveJobs[IO](xa)
          jobId    <- jobs.create("sabuj@allevite.com", AlleviteNewJob)
          maybeJob <- jobs.find(jobId)
        } yield maybeJob
        program.asserting(_.map(_.jobInfo) shouldBe None)
      }
    }

    "should activate a new job" in {
      transactor.use { xa =>
        val program = for {
          jobs <- LiveJobs[IO](xa)
          jobId <- jobs.create("sabuj@allevite.com", AlleviteNewJob)
          _ <- jobs.delete(jobId)
          maybeJob <- jobs.find(jobId)
        } yield maybeJob
        program.asserting(_.map(_.jobInfo) shouldBe Some(AlleviteNewJob))
      }
    }

    "should return an updated job if it exists" in {
      transactor.use { xa =>
        val program = for {
          jobs            <- LiveJobs[IO](xa)
          maybeUpdatedJob <- jobs.update(AwesomeJobUuid, UpdatedAwesomeJob.jobInfo)
        } yield maybeUpdatedJob
        program.asserting(_ shouldBe Some(UpdatedAwesomeJob))
      }
    }

    "should return None when trying to update a job that doesn't exist" in {
      transactor.use { xa =>
        val program = for {
          jobs            <- LiveJobs[IO](xa)
          maybeUpdatedJob <- jobs.update(NotFoundJobUuid, UpdatedAwesomeJob.jobInfo)
        } yield maybeUpdatedJob
        program.asserting(_ shouldBe None)
      }
    }

    "should delete an existing job" in {
      transactor.use { xa =>
        val program = for {
          jobs                <- LiveJobs[IO](xa)
          numberOfDeletedJobs <- jobs.delete(AwesomeJobUuid)
          countOfJobs <- sql"SELECT COUNT(*) FROM jobs WHERE id = $AwesomeJobUuid"
            .query[Int]
            .unique
            .transact(xa)
        } yield (numberOfDeletedJobs, countOfJobs)
        program.asserting {
          case (numberOfDeletedJobs, countOfJobs) => {
            numberOfDeletedJobs shouldBe 1
            countOfJobs shouldBe 0
          }
        }
      }
    }

    "should return zero updated rows if the job id to delete is not found" in {
      transactor.use { xa =>
        val program = for {
          jobs                <- LiveJobs[IO](xa)
          numberOfDeletedJobs <- jobs.delete(NotFoundJobUuid)
        } yield numberOfDeletedJobs
        program.asserting(_ shouldBe 0)
      }
    }

    "should filter remote jobs" in {
      transactor.use { xa =>
        val program = for {
          jobs         <- LiveJobs[IO](xa)
          filteredJobs <- jobs.all(JobFilter(remote = true), Pagination.default)
        } yield filteredJobs
        program.asserting(_ shouldBe List())
      }
    }

    "should filter jobs by tags" in {
      transactor.use { xa =>
        val program = for {
          jobs <- LiveJobs[IO](xa)
          filteredJobs <- jobs.all(JobFilter(tags = List("scala", "cats", "zio")), Pagination.default)
        } yield filteredJobs
        program.asserting(_ shouldBe List(AwesomeJob))
      }
    }

    "should surface a compressive filter out of all job contained" in {
      transactor.use { xa =>
        val program = for {
          jobs <- LiveJobs[IO](xa)
          filter <- jobs.possibleFilters()
        } yield filter

        program.asserting {
          case JobFilter(companies, locations, countries, seniorities, tags, maxSalary, remote) =>
            companies shouldBe List("Awesome Company")
            locations shouldBe List("Berlin")
            countries  shouldBe List("Germany")
            seniorities shouldBe List("Senior")
            tags.toSet shouldBe Set("scala", "scala-3", "cats")
            maxSalary shouldBe Some(3000)
        }
      }
    }

  }
}
