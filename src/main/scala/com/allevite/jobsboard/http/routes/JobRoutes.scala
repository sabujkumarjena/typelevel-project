package com.allevite.jobsboard.http.routes

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import cats.*
import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import com.allevite.jobsboard.logging.syntax.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.domain.pagination.*
import com.allevite.jobsboard.http.responses.*
import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.http.validation.syntax.*

import java.util.UUID
import scala.collection.mutable
class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F]) extends HttpValidationDsl[F] {

  object OffsetQueryParam extends OptionalQueryParamDecoderMatcher[Int]("offset")
  object LimitQueryParam  extends OptionalQueryParamDecoderMatcher[Int]("limit")
  // POST /jobs?limit=x&offset=y {filters} //TODO add query params and filters later
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root :? LimitQueryParam(limit) +& OffsetQueryParam(offset) =>
      for {
        filter  <- req.as[JobFilter]
        jobList <- jobs.all(filter, Pagination(limit, offset))
        resp    <- Ok(jobList)
      } yield resp
  }

  // GET /jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    jobs.find(id).flatMap {
      case Some(job) => Ok(job)
      case None => NotFound(FailureResponse(s"Job $id not found")) // NotFound(sJob $id not found")
    }
  }

  // refined
  // checked at compile time - increase compile time
  // lowers Developer Experience

  // POST /jobs/create {jobInfo}

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "create" =>
      req.validate[JobInfo] { jobInfo =>
        for {
          jobId <- jobs.create("TODO@allevite.com", jobInfo)
          resp  <- Created(jobId)
        } yield resp
      }
  }

  // PUT /jobs/uuid {jobInfo}
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      req.validate[JobInfo] { jobInfo =>
        for {

          mayBeNewJob <- jobs.update(id, jobInfo)
          resp <- mayBeNewJob match {
            case Some(job) => Ok()
            case None => NotFound(FailureResponse(s"Not able to update job .. $id : not found "))
          }
        } yield resp
      }
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      jobs.find(id).flatMap {
        case Some(job) =>
          for {
            _    <- jobs.delete(id)
            resp <- Ok()

          } yield resp

        case None => NotFound(FailureResponse(s"Not able to delete job .. $id : not found "))
      }

  }

  val routes = Router(
    "/jobs" -> (allJobsRoute <+> findJobRoute <+> createJobRoute <+> updateJobRoute <+> deleteJobRoute)
  )
}
object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F]) = new JobRoutes[F](jobs)
}
