package com.allevite.jobsboard.pages

import cats.effect.*
import com.allevite.jobsboard.pages.Page.*
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import org.scalajs.dom.{console, document, window}
import com.allevite.jobsboard.common.*
import com.allevite.jobsboard.domain.auth.*

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
) extends Page {
  import SignUpPage.*
  override def initCmd: Cmd[IO, Page.Msg] = Cmd.None

  override def update(msg: Page.Msg): (Page, Cmd[IO, Page.Msg]) = msg match {
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
        (this, Logger.consoleLog[IO]("SIGNING UP!", email, password, firstName, lastName, company))
    case _ => (this, Cmd.None)
  }

  override def view(): Html[Page.Msg] =
    div(`class` := "form-section")(
      // title: Sign Up
      div(`class` := "top-section")(
        h1("Sign UP")
      ),
      // form
      form(
        name    := "signin",
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
        button(`type` := "button", onClick(AttemptSignUp))("Sign Up"),
        status.map(s => div(s.message)).getOrElse(div())
      )
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
}

object SignUpPage {
  trait Msg                                                 extends Page.Msg
  case class UpdateEmail(email: String)                     extends Msg
  case class UpdatePassword(password: String)               extends Msg
  case class UpdateConfirmPassword(confirmPassword: String) extends Msg

  case class UpdateFirstName(firstName: String) extends Msg
  case class UpdateLastName(lastName: String)   extends Msg
  case class UpdateCompany(company: String)     extends Msg

  // actions
  case object AttemptSignUp extends Msg
  case object NoOp          extends Msg

  object Commands {
    def signup(newUserInfo: NewUserInfo): Cmd[IO, Msg ] = {
      ???
    }
  }

}
