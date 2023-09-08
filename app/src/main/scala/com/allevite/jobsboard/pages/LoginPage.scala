package com.allevite.jobsboard.pages

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.Encoder
import cats.effect.*
import com.allevite.jobsboard.pages.Page
import tyrian.*
import tyrian.Html.*
import tyrian.http.*
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.domain.auth.*
/*
form
  - email
  - password
  - button

status (success or failure)
 */
case class LoginPage(
    email: String = "",
    password: String = "",
    status: Option[Page.Status] = None
) extends Page {
  import LoginPage.*
  override def initCmd: Cmd[IO, Page.Msg] = Cmd.None

  override def update(msg: Page.Msg): (Page, Cmd[IO, Page.Msg]) = msg match {
    case UpdateEmail(email)       => (this.copy(email = email), Cmd.None)
    case UpdatePassword(password) => (this.copy(password = password), Cmd.None)
    case AttemptLogin =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Email is invalid"), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Please enter a password"), Cmd.None)
      else
        (
          this,
          Commands.login(
            LoginInfo(
              email,
              password
            )
          )
        )
    case LoginError(message) => (setErrorStatus(message), Cmd.None)
    case LoginSuccess(token) => (setSuccessStatus(token), Cmd.None)
    case _ => (this, Cmd.None)
  }

  override def view(): Html[Page.Msg] =
    div(`class` := "form-section")(
      // title: Sign Up
      div(`class` := "top-section")(
        h1("Log In")
      ),
      // form
      form(
        name    := "login",
        `class` := "form",
        onEvent(
          "submit",
          e => {
            e.preventDefault()
            NoOp
          }
        )
      )(
        // 6 inputs
        renderInput("Email", "email", "text", true, UpdateEmail(_)),
        renderInput("Password", "password", "password", true, UpdatePassword(_)),

        // button
        button(`type` := "button", onClick(AttemptLogin))("Log In")
      ),
      status.map(s => div(s.message)).getOrElse(div())
    )

  ////////////////////////////////////
  //// private
  //////////////////////

  // UI
  private def renderInput(
      name: String,
      uid: String,
      kind: String,
      isRequired: Boolean,
      onChange: String => Msg
  ) =
    div(`class` := "form-input")(
      label(`for` := name, `class` := "form-label")(
        if (isRequired) span("*") else span(),
        text(name)
      ),
      input(`type` := kind, `class` := "form-control", id := uid, onInput(onChange))
    )

  // util
  def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))

}

object LoginPage {
  trait Msg extends Page.Msg

  case class UpdateEmail(email: String) extends Msg

  case class UpdatePassword(password: String) extends Msg
//action
  case object AttemptLogin extends Msg

  case object NoOp extends Msg

  // statuses
  case class LoginError(message: String) extends Msg

  case class LoginSuccess(message: String) extends Msg

  object Commands {
    def login(loginInfo: LoginInfo): Cmd[IO, Msg] =
      Endpoints.login.call(loginInfo)

  }

  object Endpoints {
    val login = new Endpoint[Msg] {
      override val location: String = Constants.Endpoints.login
      override val method: Method   = Method.Post
      override val onSuccess: Response => Msg = response => {
        val maybeToken = response.headers.get("authorization")
        maybeToken match {
          case Some(token) => LoginSuccess(token)
          case None        => LoginError("Invalid username or password")
        }
      }
      override val onError: HttpError => Msg = e => LoginError(e.toString)

    }
  }
}
