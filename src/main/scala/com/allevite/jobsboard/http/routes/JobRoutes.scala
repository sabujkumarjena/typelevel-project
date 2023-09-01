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
import com.allevite.jobsboard.domain.job.{Job, JobInfo}
import com.allevite.jobsboard.http.responses.*

import java.util.UUID
import scala.collection.mutable
class JobRoutes[F[_]: Concurrent] private extends Http4sDsl[F] {

  // "database"
  private val database = mutable.Map[UUID, Job]()

  // POST /jobs?offset=x&limit=y {filters} //TODO add query params and filters later
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] { case POST -> Root =>
    Ok(database.values)
  }

  // GET /jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    database.get(id) match {
      case Some(job) => Ok(job)
      case None      => NotFound(FailureResponse("error")) // NotFound(sJob $id not found")
    }
  }

  // POST /jobs/create {jobInfo}
  private def createJob(jobInfo: JobInfo): F[Job] =
    Job(
      id = UUID.randomUUID(),
      date = System.currentTimeMillis(),
      ownerEmail = "TODO@allevite.com",
      jobInfo = jobInfo,
      active = true
    ).pure[F]

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "create" =>
      for {
        jobInfo <- req.as[JobInfo]
        job     <- createJob(jobInfo)
        resp    <- Created(job.id)
      } yield resp
  }

  // PUT /jobs/uuid {jobInfo}
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      database.get(id) match {
        case Some(job) =>
          for {
            jobInfo <- req.as[JobInfo]
            _       <- database.put(id, job.copy(jobInfo = jobInfo)).pure[F]
            resp    <- Ok()

          } yield resp

        case None => NotFound(FailureResponse(s"Not able to update job .. $id : not found "))
      }
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      database.get(id) match {
        case Some(job) =>
          for {
            _    <- database.remove(id).pure[F]
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
  def apply[F[_]: Concurrent] = new JobRoutes[F]
}
