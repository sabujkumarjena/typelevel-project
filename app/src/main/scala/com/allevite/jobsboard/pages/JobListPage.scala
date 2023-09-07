package com.allevite.jobsboard.pages

import tyrian.*
import tyrian.Html.*
import cats.effect.*
import Page.*
case class JobListPage() extends Page {
  override def initCmd: Cmd[IO, Msg] = Cmd.None

  override def update(msg: Msg): (Page, Cmd[IO, Msg]) = (this, Cmd.None)

  override def view(): Html[Msg] =
    div("Job List page - TODO")

}
