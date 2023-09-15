package com.allevite.jobsboard.pages

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder
import cats.effect.*
import com.allevite.jobsboard.pages.*
import tyrian.*
import tyrian.Html.*
import tyrian.http.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.domain.auth.*
import com.allevite.jobsboard.common.*
import tyrian.http.{HttpError, Method, Response}

//email, token, new password + button
case class ResetPasswordPage(
    email: String = "",
    token: String = "",
    password: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Reset Password", None) {
  import ResetPasswordPage.*
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e)    => (this.copy(email = e), Cmd.None)
    case UpdateToken(t)    => (this.copy(token = t), Cmd.None)
    case UpdatePassword(p) => (this.copy(password = p), Cmd.None)
    case AttemptResetPassword =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Email is invalid"), Cmd.None)
      else if (token.isEmpty)
        (setErrorStatus("Please enter the recieved token "), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Please enter a password"), Cmd.None)
      else
        (
          this,
          Commands.reset(
            RecoverPasswordInfo(
              email,
              token,
              password
            )
          )
        )
    case ResetPasswordFailure(error) => (setErrorStatus(error), Cmd.None)
    case ResetPasswordSuccess        => (setSuccessStatus("Success! You can login now"), Cmd.None)
    case _                           => (this, Cmd.None)
  }

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Email", "email", "text", true, UpdateEmail(_)),
    renderInput("Token", "token", "text", true, UpdateToken(_)),
    renderInput("Password", "password", "password", true, UpdatePassword(_)),
    button(`type` := "button", onClick(AttemptResetPassword))("Set Password"),
    renderAuxLink(Page.Urls.FORGOT_PASSWORD, "Don't have a token yet? ")
  )

  // utils
  def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object ResetPasswordPage {
  trait Msg                                      extends App.Msg
  case class UpdateEmail(email: String)          extends Msg
  case class UpdateToken(token: String)          extends Msg
  case class UpdatePassword(password: String)    extends Msg
  case object AttemptResetPassword               extends Msg
  case class ResetPasswordFailure(error: String) extends Msg
  case object ResetPasswordSuccess               extends Msg

  object EndPoints {
    val resetPassword = new Endpoint[Msg] {
      override val location: String = Constants.endpoints.resetPassword
      override val method: Method   = Method.Post
      override val onResponse: Response => Msg = response =>
        response.status match {
          case Status(200, _) => ResetPasswordSuccess
          case Status(s, _) if s >= 400 && s < 500 =>
            val json   = response.body
            val parsed = parse(json).flatMap(_.hcursor.get[String]("error"))
            parsed match {
              case Left(e) => ResetPasswordFailure(s"Response error: ${e.getMessage}")
              case Right(errorFromServer) => ResetPasswordFailure(errorFromServer)
            }
          case _ => ResetPasswordFailure(" unknown reply from server. Something fishy..")
        }
      override val onError: HttpError => Msg = e => ResetPasswordFailure(e.toString)
    }
  }

  object Commands {

    def reset(recoverPasswordInfo: RecoverPasswordInfo): Cmd[IO, Msg] =
      EndPoints.resetPassword.call(recoverPasswordInfo)

  }
}
