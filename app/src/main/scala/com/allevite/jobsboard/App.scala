package com.allevite.jobsboard

import cats.effect.*

import scala.scalajs.js.annotation.*
import org.scalajs.dom.document
import tyrian.*
import tyrian.Html.*
import scala.concurrent.duration.*
@JSExportTopLevel("AlleViteApp")
class App extends TyrianApp[Int, String] {
  //                     message  model = "state"

  /*
    We can send messages by
    - trigger a command
    - create a subscription
    - listening for an event
   */
  override def init(flags: Map[String, String]): (String, Cmd[IO, Int]) =
    ("", Cmd.None)

  // potentially endless stream of messages
  override def subscriptions(model: String): Sub[IO, Int] =
    Sub.None
  // Sub.every[IO](1.second).map(_ => 1)

  // model can change by recieving message
  // model => message => (new Model, _____)
  // update triggered whenever we get a new message
  override def update(model: String): Int => (String, Cmd[IO, Int]) =
    message => (model + ", " + message, Cmd.None)

  // view triggered whenever model changes
  override def view(model: String): Html[Int] =
    div(
      button(onClick(1))("click me"),
      div(s"Tyrian running: $model")
    )
}
