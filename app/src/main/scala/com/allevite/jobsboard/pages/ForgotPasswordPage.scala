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
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.domain.auth.*
import tyrian.http.{HttpError, Method, Response}
case class ForgotPasswordPage(email: String = "", status: Option[Page.Status] = None)
    extends FormPage("Reset Password", status) {

  import ForgotPasswordPage.*

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Email", "email", "text", true, UpdateEmail(_)),
    button(`type` := "button", onClick(AttemptResetPassword))("Send Email"),
    renderAuxLink(Page.Urls.RESET_PASSWORD, "Have a token?")
  )
  // override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e) => (this.copy(email = e), Cmd.None)
    case AttemptResetPassword =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus(" Please enter a valid emil "), Cmd.None)
      else
        (this, Commands.resetPassword(email))

    case ResetSuccess    => (setSuccessStatus("Check your email"), Cmd.None)
    case ResetFailure(e) => (setErrorStatus(e), Cmd.None)
    case _               => (this, Cmd.None)
  }

  // private api
  def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object ForgotPasswordPage {
  trait Msg                              extends App.Msg
  case class UpdateEmail(email: String)  extends Msg
  case object AttemptResetPassword       extends Msg
  case class ResetFailure(error: String) extends Msg
  case object ResetSuccess               extends Msg

  object Endpoints {
    val resetPassword = new Endpoint[Msg] {
      override val location: String            = Constants.endpoints.forgotPassword
      override val method: Method              = Method.Post
      override val onResponse: Response => Msg = _ => ResetSuccess
      override val onError: HttpError => Msg   = e => ResetFailure(e.toString)
    }
  }

  object Commands {
    def resetPassword(email: String) = Endpoints.resetPassword.call(ForgotPasswordInfo(email))
  }

}
