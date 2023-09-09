package com.allevite.jobsboard.pages

import tyrian.*
import cats.effect.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.*
object Page {
  trait Msg
  enum StatusKind {
    case SUCCESS, ERROR, LOADING
  }
  case class Status(message: String, kind: StatusKind)
  object Urls {
    val LOGIN            = "/login"
    val SIGNUP           = "/signup"
    val FORGOT_PASSWORD  = "/forgotpassword"
    val RECOVER_PASSWORD = "/recoverpassword"
    val JOBS             = "/jobs"
    val EMPTY            = ""
    val HOME             = "/"
    val LOGOUT           = "/jobs"
    val HASH             = "/#"

  }

  import Urls.*
  def get(location: String) = location match {
    case `LOGIN`                   => LoginPage()
    case `SIGNUP`                  => SignUpPage()
    case `FORGOT_PASSWORD`         => ForgotPasswordPage()
    case `RECOVER_PASSWORD`        => RecoverPasswordPage()
    case `EMPTY` | `HOME` | `JOBS` => JobListPage()
    //  case s"${`JOBS`}/$id"          => JobPage(id)
    case s"/jobs/$id" => JobPage(id)
    case _            => NotFoundPage()
  }
}
abstract class Page {
  // import Page.*
//API
//send a command upon instantiating
  def initCmd: Cmd[IO, App.Msg]
// update
  def update(msg: App.Msg): (Page, Cmd[IO, App.Msg])
  def view(): Html[App.Msg]
}

//login page
//signup page
//recover password page
//forgot password page
//job list page == home page
//not found page
