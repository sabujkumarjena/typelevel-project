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
import tsec.authentication.{SecuredRequestHandler, asAuthed}
import com.allevite.jobsboard.logging.syntax.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.domain.pagination.*
import com.allevite.jobsboard.http.responses.*
import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.domain.security.*
import com.allevite.jobsboard.domain.user.*
import com.allevite.jobsboard.http.validation.syntax.*

import java.util.UUID
import scala.collection.mutable
import scala.language.implicitConversions
class JobRoutes[F[_]: Concurrent: Logger: SecuredHandler] private (jobs: Jobs[F], stripe: Stripe[F])
    extends HttpValidationDsl[F] {

  object OffsetQueryParam extends OptionalQueryParamDecoderMatcher[Int]("offset")
  object LimitQueryParam  extends OptionalQueryParamDecoderMatcher[Int]("limit")

  // GET /jobs/filters => {filters}
  private val allFiltersRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "filters" =>
    jobs.possibleFilters().flatMap(jf => Ok(jf))
  }

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

  private val createJobRoute: AuthRoute[F] = { case req @ POST -> Root / "create" asAuthed user =>
    req.request.validate[JobInfo] { jobInfo =>
      for {
        jobId <- jobs.create(user.email, jobInfo)
        resp  <- Created(jobId)
      } yield resp
    }
  }

  // PUT /jobs/uuid {jobInfo}
  private val updateJobRoute: AuthRoute[F] = { case req @ PUT -> Root / UUIDVar(id) asAuthed user =>
    req.request.validate[JobInfo] { jobInfo =>
      jobs.find(id).flatMap {
        case None => NotFound(FailureResponse(s"Not able to update job .. $id : not found "))
        case Some(job) if user.owns(job) || user.isAdmin =>
          jobs.update(id, jobInfo) >> Ok()
        case _ =>
          Forbidden(
            FailureResponse(s"You can only update your own jobs or you should be admin user")
          )

      }
    }
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: AuthRoute[F] = {
    case req @ DELETE -> Root / UUIDVar(id) asAuthed user =>
      jobs.find(id).flatMap {
        case None => NotFound(FailureResponse(s"Not able to delete job .. $id : not found "))
        case Some(job) if user.owns(job) || user.isAdmin =>
          jobs.delete(id) >> Ok()
        case _ =>
          Forbidden(
            FailureResponse(s"You can only delete your own jobs or you should be admin user")
          )

      }

  }

  // Stripe Endpoints
  // POST /jobs/promoted {jobInfo}
  private val promotedJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "promoted"  =>
    req.validate[JobInfo] { jobInfo =>
      for {
        jobId <- jobs.create("TODO@allevite.com", jobInfo)
        session <- stripe.createCheckoutSession(jobId.toString, "TODO@allevite.com")
        resp  <- session.map(sess => Ok(sess.getUrl())).getOrElse(NotFound())
      } yield resp
    }
  }

  val unauthedRoutes = allFiltersRoute <+> allJobsRoute <+> findJobRoute
  val authedRoutes = SecuredHandler[F].liftService(
    createJobRoute.restrictedTo(allRoles) |+|
      updateJobRoute.restrictedTo(allRoles) |+|
      deleteJobRoute.restrictedTo(allRoles)
  )

  val routes = Router(
    "/jobs" -> (unauthedRoutes <+> authedRoutes)
  )
}
object JobRoutes {
  def apply[F[_]: Concurrent: Logger: SecuredHandler](jobs: Jobs[F], stripe: Stripe[F]) =
    new JobRoutes[F](jobs, stripe)
}
