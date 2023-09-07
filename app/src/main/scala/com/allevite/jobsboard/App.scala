package com.allevite.jobsboard

import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import cats.effect.*

import scala.scalajs.js.annotation.*
import org.scalajs.dom.{console, document, window}

import scala.concurrent.duration.*
import core.*
import com.allevite.jobsboard.components.*
import com.allevite.jobsboard.pages.*

object App {

  type Msg = Router.Msg | Page.Msg
  // type Msg = Router.Msg
  case class Model(router: Router, page: Page)

}
@JSExportTopLevel("AlleViteApp")
class App extends TyrianApp[App.Msg, App.Model] {
  import App.*

  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = {
    val location            = window.location.pathname
    val page                = Page.get(location)
    val pageCmd             = page.initCmd
    val (router, routerCmd) = Router.startAt(location)
    (Model(router, page), routerCmd |+| pageCmd)
  }

  // potentially endless stream of messages
  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub // listener for browser history changes
      .make("urlChange", model.router.history.state.discrete)
      .map(_.get)
      .map(newLocation => Router.ChangeLocation(newLocation, true))

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case msg: Router.Msg =>
      val (newRouter, routerCmd) = model.router.update(msg)
      if (model.router == newRouter) // no change necessary
        (model, Cmd.None)
      else {
        // location changed, need to re-render the appropriate page
        val newPage    = Page.get(newRouter.location)
        val newPageCmd = newPage.initCmd
        (model.copy(router = newRouter, page = newPage), routerCmd |+| newPageCmd)
      }


    case msg: Page.Msg =>
      // update the page
      val (newPage, cmd) = model.page.update(msg)
      (model.copy(page = newPage), cmd)
  }
  // view triggered whenever model changes
  override def view(model: Model): Html[Msg] =
    div(
      Header.view(),
      model.page.view()
    )

}
