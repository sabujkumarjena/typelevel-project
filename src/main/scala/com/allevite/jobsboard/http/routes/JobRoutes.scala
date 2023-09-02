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
import com.allevite.jobsboard.domain.job.{Job, JobInfo}
import com.allevite.jobsboard.http.responses.*
import com.allevite.jobsboard.core.*

import java.util.UUID
import scala.collection.mutable
class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F]) extends Http4sDsl[F] {

  // POST /jobs?offset=x&limit=y {filters} //TODO add query params and filters later
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    for {
      jobList <- jobs.all()
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

  // POST /jobs/create {jobInfo}

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "create" =>
      for {
        jobInfo <- req.as[JobInfo].logError(e => s"Parsing payload failed: $e")
        jobId   <- jobs.create("TODO@allevite.com", jobInfo)
        resp    <- Created(jobId)
      } yield resp
  }

  // PUT /jobs/uuid {jobInfo}
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      for {
        jobInfo     <- req.as[JobInfo]
        mayBeNewJob <- jobs.update(id, jobInfo)
        resp <- mayBeNewJob match {
          case Some(job) => Ok()
          case None      => NotFound(FailureResponse(s"Not able to update job .. $id : not found "))
        }
      } yield resp
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
