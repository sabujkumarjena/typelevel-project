package com.allevite.jobsboard.pages

import cats.effect.*
import com.allevite.jobsboard.pages.Page.*
import tyrian.*
import tyrian.Html.*

case class ForgotPasswordPage() extends Page {
  override def initCmd: Cmd[IO, Msg] = Cmd.None

  override def update(msg: Msg): (Page, Cmd[IO, Msg]) = (this, Cmd.None)

  override def view(): Html[Msg] =
    div("Forgot password page - TODO")

}
