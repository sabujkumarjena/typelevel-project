package com.allevite.jobsboard.http.routes

import cats.effect.*
import cats.implicits.*
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
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.domain.pagination.Pagination
import com.stripe.model.checkout.Session
import org.http4s.HttpRoutes
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger

import java.util.UUID
import com.stripe.model.checkout.Session
import com.stripe.param.checkout.SessionCreateParams
class JobRoutesSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with JobFixture
    with SecuredRouteFixture {
  ////////////////
//prep
//////////////////
  val jobs: Jobs[IO] = new Jobs[IO] {
    override def create(ownerEmail: String, jobInfo: JobInfo): IO[UUID] =
      IO.pure(NewJobUuid)
    override def all(filter: JobFilter, pagination: Pagination): IO[List[Job]] =
      if (filter.remote) IO.pure(List())
      else IO.pure(List(AwesomeJob))
    override def all(): IO[List[Job]] = IO.pure(List(AwesomeJob))
    override def find(id: UUID): IO[Option[Job]] =
      if (id == AwesomeJobUuid) IO.pure(Some(AwesomeJob))
      else IO.pure(None)
    override def update(id: UUID, jobInfo: JobInfo): IO[Option[Job]] =
      if (id == AwesomeJobUuid) IO.pure(Some(UpdatedAwesomeJob))
      else IO.pure(None)
    override def delete(id: UUID): IO[Int] =
      if (id == AwesomeJobUuid) IO.pure(1)
      else IO.pure(0)
    override def possibleFilters(): IO[JobFilter] = IO(
      defaultFilter
    )

  override def activate(id: UUID): IO[Int] = IO.pure(1)
  }

  val stripe: Stripe[IO]  = new LiveStripe[IO] ("key", "price", "example.com/test" , "example.com/fail", "secret" ){
    override def createCheckoutSession(jobId: String, userEmail: String): IO[Option[Session]] =
      IO.pure(Some(Session.create(SessionCreateParams.builder().build())))

    override def handleWebhookEvent[A](payload: String, signature: String, action: String => IO[A]): IO[Option[A]] =
      IO.pure(None)
  }

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  // this is what we are testing
  val jobsRoutes: HttpRoutes[IO] = JobRoutes[IO](jobs, stripe).routes
  val defaultFilter: JobFilter = JobFilter(companies = List("Awesome Company"))
  ////////////////////////
  // tests
  ////////////////////

  "JobRoutes" - {
    "should return a job with a given id" in {
      for {
        response <- jobsRoutes.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
        )
        retrieved <- response.as[Job]

      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe AwesomeJob
      }
    }

    "should return all jobs" in {
      for {
        response <- jobsRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/jobs")
            .withEntity(JobFilter()) // empty filter
        )
        retrieved <- response.as[List[Job]]

      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(AwesomeJob)
      }
    }

    "should return all jobs that satisfy a filter " in {
      for {
        response <- jobsRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/jobs")
            .withEntity(JobFilter(remote = false))
        )
        retrieved <- response.as[List[Job]]

      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(AwesomeJob)
      }
    }

    "should create a new job" in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        response <- jobsRoutes.orNotFound.run(
          Request[IO](method = Method.POST, uri = uri"/jobs/create")
            .withEntity(AwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
        retrieved <- response.as[UUID]

      } yield {
        response.status shouldBe Status.Created
        retrieved shouldBe NewJobUuid
      }
    }

    "should only update a job that exists" in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        responseOk <- jobsRoutes.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withEntity(UpdatedAwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
        responseInvalid <- jobsRoutes.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40065")
            .withEntity(UpdatedAwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseInvalid.status shouldBe Status.NotFound
      }
    }

    "should forbid update of job that the JWT token doen't 'own'" in {
      for {
        jwtToken <- mockedAuthenticator.create("somebody@gmail.com")
        response <- jobsRoutes.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withEntity(UpdatedAwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should only delete a job that exists" in {
      for {
        jwtToken <- mockedAuthenticator.create(sabujEmail)
        responseOk <- jobsRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withBearerToken(jwtToken)
        )
        responseInvalid <- jobsRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40065")
            .withBearerToken(jwtToken)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseInvalid.status shouldBe Status.NotFound
      }
    }

    "should surface all possible filters" in {
      for {
        response <- jobsRoutes.orNotFound.run(
          Request(method = Method.GET, uri=uri"/jobs/filters")
        )
        filter <- response.as[JobFilter]
      } yield {
        filter shouldBe defaultFilter
      }
    }

  }

}
