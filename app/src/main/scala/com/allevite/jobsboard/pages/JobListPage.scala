package com.allevite.jobsboard.pages

import tyrian.*
import tyrian.Html.*
import cats.effect.*
import Page.*
case class JobListPage() extends Page {
  override def initCmd: Cmd[IO, Page.Msg] = Cmd.None

  override def update(msg: Page.Msg): (Page, Cmd[IO, Page.Msg]) = (this, Cmd.None)

  override def view(): Html[Page.Msg] =
    div("Job List page - TODO")

}
