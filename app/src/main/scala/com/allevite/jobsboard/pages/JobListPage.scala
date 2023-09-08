package com.allevite.jobsboard.pages

import tyrian.*
import tyrian.Html.*
import cats.effect.*
import Page.*
import com.allevite.jobsboard.*
case class JobListPage() extends Page {
  override def initCmd: Cmd[IO, App.Msg] = Cmd.None

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = (this, Cmd.None)

  override def view(): Html[App.Msg] =
    div("Job List page - TODO")

}
