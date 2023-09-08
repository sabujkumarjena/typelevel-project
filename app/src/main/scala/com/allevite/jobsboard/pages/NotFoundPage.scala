package com.allevite.jobsboard.pages

import cats.effect.*
import com.allevite.jobsboard.pages.Page.*
import tyrian.*
import tyrian.Html.*
import com.allevite.jobsboard.*

case class NotFoundPage() extends Page {
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = (this, Cmd.None)

  override def view(): Html[App.Msg] =
    div("Job List page - TODO")

}
