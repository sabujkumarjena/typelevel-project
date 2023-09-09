package com.allevite.jobsboard.components

import com.allevite.jobsboard.*
import tyrian.*
import tyrian.Html.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.pages.*
import org.scalajs.dom.Location
object Header {
  // public API
  def view() =
    div(`class` := "header-container")(
      renderLogo(),
      div(`class` := "header-nav")(
        ul(`class` := "header-links")(
          renderNavLinks()
        )
      )
    )

    // private API

  @js.native
  @JSImport("/static/img/allevitelogo.png", JSImport.Default)
  private val logoImage: String = js.native
  private def renderLogo() =
    a(
      href := "/",
      onEvent(
        "click",
        e => {
          e.preventDefault() // native JS - prevent reloading
          Router.ChangeLocation("/")
        }
      )
    )(
      img(
        `class` := "home-logo",
        src     := logoImage,
        alt     := "Allevite"
      )
    )

  private def renderNavLinks(): List[Html[App.Msg]] = {

    val constantLinks = List(renderSimpleNavLink("Jobs", Page.Urls.JOBS))
    val unauthedLinks = List(
      renderSimpleNavLink("Login", Page.Urls.LOGIN),
      renderSimpleNavLink("Sign Up", Page.Urls.SIGNUP)
    )
    val authedLinks = List(
      renderNavLink("Logout", Page.Urls.HASH)(_ => Session.Logout)
    )

    constantLinks ++ (
      if (Session.isActive) authedLinks
      else unauthedLinks
    )
  }

  private def renderSimpleNavLink(text: String, location: String) =
    renderNavLink(text, location)(Router.ChangeLocation(_))
  private def renderNavLink(text: String, location: String)(location2msg: String => App.Msg) =
    li(`class` := "nav-item")(
      a(
        href    := location,
        `class` := "nav-link",
        onEvent(
          "click",
          e => {
            e.preventDefault() // native JS - prevent reloading
            location2msg(location)
          }
        )
      )(text)
    )
}
