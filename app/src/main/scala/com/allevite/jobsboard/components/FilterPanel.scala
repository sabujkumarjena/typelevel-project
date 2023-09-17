package com.allevite.jobsboard.components

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder
import cats.effect.IO
import tyrian.*
import tyrian.Html.*
import tyrian.http.*
import com.allevite.jobsboard.App
import com.allevite.jobsboard.domain.job.JobFilter

import com.allevite.jobsboard.common.*
case class FilterPanel(
    possibleFilters: JobFilter = JobFilter(),
    maybeError: Option[String] = None,
    maxSalary: Int = 0
) extends Component[App.Msg, FilterPanel] {
  import FilterPanel.*
  override def initCmd: Cmd[IO, App.Msg] = Commands.getFilters

  override def update(msg: App.Msg): (FilterPanel, Cmd[IO, App.Msg]) = msg match {
    case SetPossibleFilters(pf)    => (this.copy(possibleFilters = pf), Cmd.None)
    case FilterPanelError(e)       => (this.copy(maybeError = Some(e)), Cmd.None)
    case UpdateSalaryInput(salary) => (this.copy(maxSalary = salary), Cmd.None)
    case _                         => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    div(`class` := "filter-panel-container")(
      maybeRenderError(),
      renderSalaryFilter()
    )

  private def renderSalaryFilter() =
    div(`class` := "filter-group")(
      h6(`class` := "filter-group-header")("Salary"),
      div(`class` := "filter-group-content")(
        label(),
        input(
          `type` := "number",
          id     := "filter-salary",
          onInput(s => UpdateSalaryInput(if (s.isEmpty) 0 else s.toInt))
        )
      )
    )

  private def maybeRenderError() =
    maybeError
      .map { e =>
        div(`class` := "filter-panel-error")(e)
      }
      .getOrElse(div())
}

object FilterPanel {
  trait Msg                                                 extends App.Msg
  case class FilterPanelError(error: String)                extends Msg
  case class SetPossibleFilters(possibleFilters: JobFilter) extends Msg
  case class UpdateSalaryInput(salary: Int)                 extends Msg
  object Endpoints {
    val getFilters = new Endpoint[Msg] {
      override val location: String          = Constants.endpoints.filters
      override val method: Method            = Method.Get
      override val onError: HttpError => Msg = e => FilterPanelError(e.toString)
      override val onResponse: Response => Msg =
        Endpoint.onResponse[JobFilter, Msg](
          SetPossibleFilters(_),
          FilterPanelError(_)
        )
    }
  }

  object Commands {
    def getFilters = Endpoints.getFilters.call()
  }
}
