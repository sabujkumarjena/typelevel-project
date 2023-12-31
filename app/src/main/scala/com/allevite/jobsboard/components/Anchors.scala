package com.allevite.jobsboard.components

import tyrian.*
import tyrian.Html.*

import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.App
object Anchors {
  def renderSimpleNavLink(text: String, location: String, cssClass: String = "") =
    renderNavLink(text, location, cssClass)(Router.ChangeLocation(_))

  def renderNavLink(text: String, location: String, cssClass: String = "")(
      location2msg: String => App.Msg
  ) =
    li(`class` := "nav-item")(
      a(
        href    := location,
        `class` := cssClass,
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
