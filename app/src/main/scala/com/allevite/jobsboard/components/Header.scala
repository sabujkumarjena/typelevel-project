package com.allevite.jobsboard.components

import tyrian.*
import tyrian.Html.*
import scala.scalajs.js
import scala.scalajs.js.annotation.*

import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.pages.*
object Header {
  // public API
  def view() =
    div(`class` := "header-container")(
      renderLogo(),
      div(`class` := "header-nav")(
        ul(`class` := "header-links")(
          renderNavLink("Jobs", Page.Urls.JOBS),
          renderNavLink("Login", Page.Urls.LOGIN),
          renderNavLink("Sign Up", Page.Urls.SIGNUP)
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
  private def renderNavLink(text: String, location: String) =
    li(`class` := "nav-item")(
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
    )
}
