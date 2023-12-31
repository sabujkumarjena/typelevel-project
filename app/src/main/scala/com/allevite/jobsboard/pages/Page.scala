package com.allevite.jobsboard.pages

import tyrian.*
import cats.effect.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.*
import com.allevite.jobsboard.components.*
object Page {
  trait Msg
  enum StatusKind {
    case SUCCESS, ERROR, LOADING
  }
  final case class Status(message: String, kind: StatusKind)
  object Status {
    val LOADING = Status("Loading", StatusKind.LOADING)
  }
  object Urls {
    val LOGIN           = "/login"
    val SIGNUP          = "/signup"
    val FORGOT_PASSWORD = "/forgotpassword"
    val RESET_PASSWORD  = "/resetpassword"
    val PROFILE         = "/profile"
    val POST_JOB        = "/postjob"
    val JOBS            = "/jobs"
    val EMPTY           = ""
    val HOME            = "/"
    val LOGOUT          = "/jobs"
    val HASH            = "/#"
    def JOB(id: String) = s"/jobs/$id"

  }

  import Urls.*
  def get(location: String) = location match {
    case `LOGIN`                   => LoginPage()
    case `SIGNUP`                  => SignUpPage()
    case `FORGOT_PASSWORD`         => ForgotPasswordPage()
    case `RESET_PASSWORD`          => ResetPasswordPage()
    case `PROFILE`                 => ProfilePage()
    case `POST_JOB`                => PostJobPage()
    case `EMPTY` | `HOME` | `JOBS` => JobListPage()
    //  case s"${`JOBS`}/$id"          => JobPage(id)
    case s"/jobs/$id" => JobPage(id)
    case _            => NotFoundPage()
  }
}
abstract class Page extends Component[App.Msg, Page]

//login page
//signup page
//recover password page
//forgot password page
//job list page == home page
//not found page
