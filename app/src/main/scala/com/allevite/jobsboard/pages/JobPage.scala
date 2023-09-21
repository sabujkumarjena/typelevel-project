package com.allevite.jobsboard.pages

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder

import cats.effect.*
import com.allevite.jobsboard.pages.Page.*
import tyrian.*
import tyrian.Html.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.components.*
import tyrian.http.*

import laika.api.*
import laika.format.*

import scala.scalajs.*

import scala.scalajs.js.* //js.native
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal()
class Moment extends js.Object {
  def fromNow(): String = js.native // actually invokes the format() function in JS.
  // surface out any JS function as a new Scala method in this class
}

@js.native
@JSImport("moment", JSImport.Default) // run a JS import statement
object MomentLib extends js.Object {
  def unix(date: Long): Moment = js.native

}

case class JobPage(
    id: String,
    maybeJob: Option[Job] = None,
    status: Page.Status = Page.Status.LOADING
) extends Page {
  import JobPage.*
  override def initCmd: Cmd[IO, App.Msg] =
    Commands.getJob(id)

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case SetError(e) => (setErrorStatus(e), Cmd.None)
    case SetJob(job) => (setSuccessStatus("Success").copy(maybeJob = Some(job)), Cmd.None)
    case _           => (this, Cmd.None)
  }
  override def view(): Html[App.Msg] = maybeJob match {
    case Some(job) => renderJobPage(job)
    case None      => renderNoJobPage()
  }

//private

  private def renderJobPage(job: Job) =
    div(`class` := "container-fluid the-rock")(
      div(`class` := "row jvm-jobs-details-top-card")(
        div(`class` := "col-md-12 p-0")(
          div(`class` := "jvm-jobs-details-card-profile-img")(
            img(
              `class` := "img-fluid",
              src     := job.jobInfo.image.getOrElse(""),
              alt     := job.jobInfo.title
            )
          ),
          div(`class` := "jvm-jobs-details-card-profile-title")(
            h1(s"${job.jobInfo.company} - ${job.jobInfo.title}"),
            div(`class` := "jvm-jobs-details-card-profile-job-details-company-and-location")(
              JobComponents.renderJobSummary(job)
            )
          ),
          div(`class` := "jvm-jobs-details-card-apply-now-btn")(
            a(href := job.jobInfo.externalUrl, target := "blank")(
              button(`type` := "button", `class` := "btn btn-warning")("Apply now")
            ),
            p(MomentLib.unix(job.date/1000).fromNow())
          )
        )
      ),
      div(`class` := "container-fluid")(
        div(`class` := "container")(
          div(`class` := "overview-section")(
            JobComponents.renderJobDescription(job)
          )
        ),
        div(`class` := "container")(
          div(`class` := "rok-last")(
            div(`class` := "row")(
              div(`class` := "col-md-6 col-sm-6 col-6")(
                span(`class` := "rock-apply")("Apply for this job.")
              ),
              div(`class` := "col-md-6 col-sm-6 col-6")(
                a(href := job.jobInfo.externalUrl, target := "blank")(
                  button(`type` := "button", `class` := "rock-apply-btn")("Apply now")
                )
              )
            )
          )
        )
      )
    )

  private def renderJobDescription(job: Job) = {
    val descriptionHtml = markdownTransformer.transform(job.jobInfo.description) match {
      case Left(e) =>
        """
          |Dammit.
           Had an error showing Markdown for this job description.
           Just hit the apply button (that shoud still work) - also let them know about the problem!
          |""".stripMargin
      case Right(html) => html
    }
    div(`class` := "job-description")().innerHtml(descriptionHtml)
  }

  private def renderJobDetails(job: Job) = {
    def renderDetail(value: String) =
      if (value.isEmpty) div()
      else li(`class` := "job-detail-value")(value)

    val fullLocationString = job.jobInfo.country match {
      case Some(country) => s"${job.jobInfo.location}, $country"
      case None          => s"${job.jobInfo.location}"
    }

    val currency = job.jobInfo.currency.getOrElse("")
    val fullSalaryString = (job.jobInfo.salaryLo, job.jobInfo.salaryHi) match {
      case (Some(lo), Some(hi)) => s"$currency $lo-$hi"
      case (Some(lo), None)     => s"> $currency $lo"
      case (None, Some(hi))     => s" upto $currency $hi"
      case _                    => "salary not specified "
    }

    div(`class` := "job-details")(
      ul(`class` := "job-detail")(
        renderDetail(fullLocationString),
        renderDetail(fullSalaryString),
        renderDetail(job.jobInfo.seniority.getOrElse("all levels")),
        renderDetail(job.jobInfo.tags.getOrElse(List()).mkString(","))
      )
    )
  }

  private def renderNoJobPage() = status.kind match {
    case Page.StatusKind.LOADING => div("Loading..")
    case Page.StatusKind.ERROR   => div("Oops!! This job doesn't exist.")
    case Page.StatusKind.SUCCESS => div(" Something is fishy..Server is healthy but no job")
  }

  // logic
  val markdownTransformer = Transformer
    .from(Markdown)
    .to(HTML)
    .build

  def setErrorStatus(message: String) =
    this.copy(status = Page.Status(message, Page.StatusKind.ERROR))

  def setSuccessStatus(message: String) =
    this.copy(status = Page.Status(message, Page.StatusKind.SUCCESS))

}

object JobPage {
  trait Msg                          extends App.Msg
  case class SetError(error: String) extends Msg
  case class SetJob(job: Job)        extends Msg
  object Endpoints {
    def getJob(id: String) = new Endpoint[Msg] {
      override val location: String          = Constants.endpoints.jobs + s"/$id"
      override val method: Method            = Method.Get
      override val onError: HttpError => Msg = e => SetError(e.toString)
      override val onResponse: Response => Msg =
        Endpoint.onResponse[Job, Msg](SetJob(_), SetError(_))
    }
  }
  object Commands {
    def getJob(id: String) = Endpoints.getJob(id).call()
  }
}
