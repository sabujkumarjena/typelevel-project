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

import java.util.UUID
import scala.collection.mutable
class JobRoutes[F[_]: Concurrent: Logger] private extends Http4sDsl[F] {

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
      case None      => NotFound(FailureResponse(s"Job $id not found")) // NotFound(sJob $id not found")
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
       // _ <- Logger[F].info("Trying to add job")
        jobInfo <- req.as[JobInfo].logError(e => s"Parsing payload failed: $e")
        //_ <- Logger[F].info(s"Parsed job info: $jobInfo")
        job     <- createJob(jobInfo)
        //_ <- Logger[F].info(s"Created job: $job")
        _ <- database.put(job.id, job).pure[F]
        resp    <- Created(job.id)
      } yield resp
  }

  // PUT /jobs/uuid {jobInfo}
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      database.get(id) match {
        case Some(job) =>
          for {
          //  _ <- Logger[F].info("Trying to update job")
            jobInfo <- req.as[JobInfo]
           // _ <- Logger[F].info(s"Parsed job info: $jobInfo")
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
  def apply[F[_]: Concurrent: Logger] = new JobRoutes[F]
}
