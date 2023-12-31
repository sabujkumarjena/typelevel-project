package com.allevite.jobsboard.pages

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import tyrian.http.*
import cats.effect.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.domain.job.*
import com.allevite.jobsboard.components.*
case class JobListPage(
    filterPanel: FilterPanel = FilterPanel(filterAction = FilterJobs(_)),
    jobFilter: JobFilter = JobFilter(),
    jobs: List[Job] = List.empty,
    canLoadMore: Boolean = true,
    status: Option[Page.Status] = Some(Page.Status.LOADING)
) extends Page {
  import JobListPage.*
  override def initCmd: Cmd[IO, App.Msg] = filterPanel.initCmd |+| Commands.getJobs()

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case SetErrorStatus(e) => (setErrorStatus(e), Cmd.None)
    case AddJobs(list, clm) =>
      (setSuccessStatus("Loaded").copy(jobs = this.jobs ++ list, canLoadMore = clm), Cmd.None)
    case LoadMoreJobs => (this, Commands.getJobs(filter = jobFilter, offset = jobs.length))
    case FilterJobs(selectedFilters) =>
      val newJobFilter = createJobFilter(selectedFilters)
      (this.copy(jobs = List(), jobFilter = newJobFilter), Commands.getJobs(filter = newJobFilter))
    case msg: FilterPanel.Msg =>
      val (newFilterPanel, cmd) = filterPanel.update(msg)
      (this.copy(filterPanel = newFilterPanel), cmd)
    case _ => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    section(`class` := "section-1")(
      div(`class` := "container")(
        div(`class` := "row jvm-recent-jobs-body")(
          div(`class` := "col-lg-4")(
            filterPanel.view()
          ),
          div(`class` := "col-lg-8")(
            jobs.map(JobComponents.card) ++ maybeRenderLoadMore
          )
        )
      )
    )

  // private

  private def maybeRenderLoadMore: Option[Html[App.Msg]] = status.map { s =>
    div(`class` := "load-more-action")(
      s match {
        case Page.Status(_, Page.StatusKind.LOADING) => div("loading...")
        case Page.Status(e, Page.StatusKind.ERROR)   => div(e)
        case Page.Status(_, Page.StatusKind.SUCCESS) =>
          if (canLoadMore)
            button(`type` := "button", `class` := "load-more-btn", onClick(LoadMoreJobs))(
              "Load more"
            )
          else
            div("All jobs loaded")

      }
    )
  }

  // util
  private def createJobFilter(selectedFilters: Map[String, Set[String]]) =
    JobFilter(
      selectedFilters.get("Companies").getOrElse(Set()).toList,
      selectedFilters.get("Locations").getOrElse(Set()).toList,
      selectedFilters.get("Countries").getOrElse(Set()).toList,
      selectedFilters.get("Seniorities").getOrElse(Set()).toList,
      selectedFilters.get("Tags").getOrElse(Set()).toList,
      Some(filterPanel.maxSalary).filter(_ > 0),
      filterPanel.remote
    )
  def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))

}

object JobListPage {

  trait Msg extends App.Msg

  case class SetErrorStatus(error: String)                         extends Msg
  case class AddJobs(list: List[Job], canLoadMore: Boolean)        extends Msg
  case object LoadMoreJobs                                         extends Msg
  case class FilterJobs(selectedFilters: Map[String, Set[String]]) extends Msg
  object Endpoints {
    def getJobs(limit: Int = Constants.defaultPageSize, offset: Int = 0) = new Endpoint[Msg] {
      override val location: String = Constants.endpoints.jobs + s"?limit=$limit&offset=$offset"
      override val method: Method   = Method.Post
      override val onError: HttpError => Msg = e => SetErrorStatus(e.toString)
      override val onResponse: Response => Msg =
        Endpoint.onResponse[List[Job], Msg](
          list => AddJobs(list, canLoadMore = offset == 0 || !list.isEmpty),
          SetErrorStatus(_)
        )
    }
  }

  object Commands {
    def getJobs(
        filter: JobFilter = JobFilter(),
        limit: Int = Constants.defaultPageSize,
        offset: Int = 0
    ): Cmd[IO, Msg] = Endpoints.getJobs(limit, offset).call(filter)
  }
}
