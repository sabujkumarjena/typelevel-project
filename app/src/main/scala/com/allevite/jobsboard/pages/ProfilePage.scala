package com.allevite.jobsboard.pages

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder
import cats.effect.*
import com.allevite.jobsboard.pages.*
import tyrian.*
import tyrian.Html.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.core.Session
import com.allevite.jobsboard.domain.auth.*
import tyrian.http.*
final case class ProfilePage(
    oldPassword: String = "",
    newPassword: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Profile", status) {
  import ProfilePage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateOP(p)                => (this.copy(oldPassword = p), Cmd.None)
    case UpdateNP(p)                => (this.copy(newPassword = p), Cmd.None)
    case AttemptCP                  => (this, Commands.changePassword(oldPassword, newPassword))
    case ChangePasswordSuccess      => (setSuccessStatus("Password Change successful"), Cmd.None)
    case ChangePasswordError(error) => (setErrorStatus(error), Cmd.None)

    case _ => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] = if (Session.isActive) super.view()
  else renderInvalidPage

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Old Password", "oldPassword", "password", true, UpdateOP(_)),
    renderInput("New Password", "newPassword", "password", true, UpdateNP(_)),
    button(`type` := "button", onClick(AttemptCP))("Change Password")
    // renderAuxLink(Page.Urls.RESET_PASSWORD, "Have a token?")
  )

  // util

  private def renderInvalidPage =
    div(
      h1("Profile"),
      div("Oops!! It seems you are not logged in yet.")
    )
  def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object ProfilePage {
  trait Msg                                     extends App.Msg
  case class UpdateOP(password: String)         extends Msg
  case class UpdateNP(password: String)         extends Msg
  case object AttemptCP                         extends Msg
  case class ChangePasswordError(error: String) extends Msg
  case object ChangePasswordSuccess             extends Msg

  object Endpoints {
    val changePasssword = new Endpoint[Msg] {
      override val location: String          = Constants.endpoints.changePassword
      override val method: Method            = Method.Put
      override val onError: HttpError => Msg = e => ChangePasswordError(e.toString)
      override val onResponse: Response => Msg = _.status match {
        case Status(200, _) => ChangePasswordSuccess
        case Status(404, _) => ChangePasswordError("Funny.Server says this user doesn't exist")
        case Status(s, _) if s >= 400 && s < 500 => ChangePasswordError("Invalid credentials")
        case _ => ChangePasswordError(" unknown reply from server. Something fishy..")
      }
    }
  }
  object Commands {
    def changePassword(oldPassword: String, newPassword: String) =
      Endpoints.changePasssword.callAuthorized(
        NewPasswordInfo(
          oldPassword,
          newPassword
        )
      )
  }
}
