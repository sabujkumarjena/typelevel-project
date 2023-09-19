package com.allevite.jobsboard.components

import tyrian.*
import tyrian.Html.*

import com.allevite.jobsboard.core.*
import com.allevite.jobsboard.App
object Anchors {
  def renderSimpleNavLink(text: String, location: String) =
    renderNavLink(text, location)(Router.ChangeLocation(_))

  def renderNavLink(text: String, location: String)(location2msg: String => App.Msg) =
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
