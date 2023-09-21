package com.allevite.jobsboard.components

import tyrian.*
import tyrian.Html.*
import com.allevite.jobsboard.App
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.pages.Page
import laika.api.*
import laika.format.*

object JobComponents {

  def card(job: Job): Html[App.Msg] =
    div(`class` := "jvm-recent-jobs-cards")(
      div(`class` := "jvm-recent-jobs-card-img")(
        img(
          `class` := "img-fluid",
          src     := job.jobInfo.image.getOrElse(""),
          alt     := job.jobInfo.title
        )
      ),
      div(`class` := "jvm-recent-jobs-card-contents")(
        h5(
          Anchors
            .renderSimpleNavLink(
              s"${job.jobInfo.company} - ${job.jobInfo.title}",
              Page.Urls.JOB(job.id.toString),
              "job-title-link"
            )
        ),
        JobComponents.renderJobSummary(job)
      ),
      div(`class` := "jvm-recent-jobs-card-btn-apply")(
        a(href := job.jobInfo.externalUrl, target := "blank")(
          button(`type` := "button", `class` := "btn btn-danger")("Apply")
        )
      )
    )

  // logic
  val markdownTransformer = Transformer
    .from(Markdown)
    .to(HTML)
    .build
  def renderJobDescription(job: Job) = {
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
  def renderJobSummary(job: Job) =
    div(`class` := "job-summary")(
      renderDetail("dollar", fullSalaryString(job)),
      renderDetail("location-dot", fullLocationString(job)),
      maybeRenderDetail("ranking-star", job.jobInfo.seniority),
      maybeRenderDetail("tags", job.jobInfo.tags.map(_.mkString(", ")))
      // tags
    )
  def maybeRenderDetail(icon: String, maybeValue: Option[String]): Html[App.Msg] =
    maybeValue.map(value => renderDetail(icon, value)).getOrElse(div())
  def renderDetail(icon: String, value: String): Html[App.Msg] =
    div(`class` := "job-detail")(
      i(`class` := s"fa fa-$icon job-detail-icon")(),
      p(`class` := "job-detail-value")(value)
    )

  // private
  private def fullLocationString(job: Job) = job.jobInfo.country match {
    case Some(country) => s"${job.jobInfo.location}, $country"
    case None          => s"${job.jobInfo.location}"
  }

  private def fullSalaryString(job: Job) = {
    val currency = job.jobInfo.currency.getOrElse("")
    (job.jobInfo.salaryLo, job.jobInfo.salaryHi) match {
      case (Some(lo), Some(hi)) => s"$currency $lo-$hi"
      case (Some(lo), None)     => s"> $currency $lo"
      case (None, Some(hi))     => s" upto $currency $hi"
      case _                    => "salary not specified "
    }
  }

}
