package com.allevite.jobsboard.pages

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import cats.effect.*
import com.allevite.jobsboard.pages.*
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import org.scalajs.dom.{console, document, window}
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.domain.auth.*
import io.circe.Encoder
import tyrian.http.*
import com.allevite.jobsboard.*

// form
/*
  input
    - email
    - password
    - confirm password
    - first name
    - last  name
    - company
  button - trigger a sign up
 */
case class SignUpPage(
    email: String = "",
    password: String = "",
    confirmPassword: String = "",
    firstName: String = "",
    lastName: String = "",
    company: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Sign Up", status) {
  import SignUpPage.*
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(email)       => (this.copy(email = email), Cmd.None)
    case UpdatePassword(password) => (this.copy(password = password), Cmd.None)
    case UpdateConfirmPassword(confirmPassword) =>
      (this.copy(confirmPassword = confirmPassword), Cmd.None)
    case UpdateFirstName(firstName) => (this.copy(firstName = firstName), Cmd.None)
    case UpdateLastName(lastName)   => (this.copy(lastName = lastName), Cmd.None)
    case UpdateCompany(company)     => (this.copy(company = company), Cmd.None)
    case AttemptSignUp =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Email is invalid"), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Please enter a password"), Cmd.None)
      else if (password != confirmPassword)
        (setErrorStatus("Password fields don't match"), Cmd.None)
      else
        (
          this,
          Commands.signup(
            NewUserInfo(
              email,
              password,
              Option(firstName).filter(_.nonEmpty),
              Option(lastName).filter(_.nonEmpty),
              Option(company).filter(_.nonEmpty)
            )
          )
        )

    case SignUpError(message)   => (setErrorStatus(message), Cmd.None)
    case SignUpSuccess(message) => (setSuccessStatus(message), Cmd.None)
    case _                      => (this, Cmd.None)
  }

  override protected def renderFormContent(): List[Html[App.Msg]] = List(

    renderInput("Email", "email", "text", true, UpdateEmail(_)),
    renderInput("Password", "password", "password", true, UpdatePassword(_)),
    renderInput(
      "Confirm password",
      "confirmpassword",
      "password",
      true,
      UpdateConfirmPassword(_)
    ),
    renderInput("First Name", "firstName", "text", false, UpdateFirstName(_)),
    renderInput("Last Name", "lastName", "text", false, UpdateLastName(_)),
    renderInput("Company", "company", "text", false, UpdateCompany(_)),
    // button
    button(`type` := "button", onClick(AttemptSignUp))("Sign Up")
  )

  ////////////////////////////////////
  //// private
  //////////////////////

  // util
  def setErrorStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String): Page =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object SignUpPage {
  trait Msg                                                 extends App.Msg
  case class UpdateEmail(email: String)                     extends Msg
  case class UpdatePassword(password: String)               extends Msg
  case class UpdateConfirmPassword(confirmPassword: String) extends Msg

  case class UpdateFirstName(firstName: String) extends Msg
  case class UpdateLastName(lastName: String)   extends Msg
  case class UpdateCompany(company: String)     extends Msg

  // actions
  case object AttemptSignUp extends Msg
  case object NoOp          extends Msg
  // statuses
  case class SignUpError(message: String)   extends Msg
  case class SignUpSuccess(message: String) extends Msg

  object Endpoints {
    val signup = new Endpoint[Msg] {
      override val location: String = Constants.endpoints.signup
      override val method: Method   = Method.Post
      override val onResponse: Response => Msg = response =>
        response.status match {
          case Status(201, _) => SignUpSuccess("Success! Log in Now. ")
          case Status(s, _) if s >= 400 && s < 500 =>
            val json   = response.body
            val parsed = parse(json).flatMap(json => json.hcursor.get[String]("error"))
            parsed match {
              case Left(e)  => SignUpError(s"Error: ${e.getMessage}")
              case Right(e) => SignUpError(e)
            }
        }
      override val onError: HttpError => Msg = e => SignUpError(e.toString)

    }
  }
  object Commands {
    def signup(newUserInfo: NewUserInfo): Cmd[IO, Msg] =
      Endpoints.signup.call(newUserInfo)

  }

}
