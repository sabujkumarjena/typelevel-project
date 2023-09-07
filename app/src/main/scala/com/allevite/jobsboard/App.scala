package com.allevite.jobsboard

import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import cats.effect.*

import scala.scalajs.js.annotation.*
import org.scalajs.dom.{window, console, document}

import scala.concurrent.duration.*
import core.*

object App {

  type Msg = Router.Msg
  // type Msg = Router.Msg
  case class Model(router: Router)

}
@JSExportTopLevel("AlleViteApp")
class App extends TyrianApp[App.Msg, App.Model] {
  import App.*

  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = {
    val (router, cmd) = Router.startAt(window.location.pathname)
    (Model(router), cmd)
  }

  // potentially endless stream of messages
  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub // listener for browser history changes
      .make("urlChange", model.router.history.state.discrete)
      .map(_.get)
      .map(newLocation => Router.ChangeLocation(newLocation, true))

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = { case msg: Router.Msg =>
    val (newRouter, cmd) = model.router.update(msg)
    (model.copy(router = newRouter), cmd)
  }
  // view triggered whenever model changes
  override def view(model: Model): Html[Msg] =
    div(
      renderNavLink("Jobs", "/jobs"),
      renderNavLink("Login", "/login"),
      renderNavLink("Sign Up", "/signup"),
      div(s"You are now at : ${model.router.location}")
    )

  private def renderNavLink(text: String, location: String) =
    a(
      href    := location,
      `class` := "nav-link",
      onEvent(
        "click",
        e => {
          e.preventDefault() // native JS - prevent reloading
          Router.ChangeLocation(location)
        }
      )
    )(text)
}
