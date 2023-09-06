package com.allevite.jobsboard

import cats.effect.*

import scala.scalajs.js.annotation.*
import org.scalajs.dom.{console, document}
import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger

import scala.concurrent.duration.*

object App {
  sealed trait Msg
  case class Increament(amount: Int) extends Msg

  case class Model(count: Int)

}
@JSExportTopLevel("AlleViteApp")
class App extends TyrianApp[App.Msg, App.Model] {
  //                     message  model = "state"
  import App.*

  /*
    We can send messages by
    - trigger a command
    - create a subscription
    - listening for an event
   */
  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (Model(0), Cmd.None)

  // potentially endless stream of messages
  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None
  // Sub.every[IO](1.second).map(_ => 1)

  // model can change by recieving message
  // model => message => (new Model, _____)
  // update triggered whenever we get a new message
  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = { case Increament(amount) =>
    // console.log("Changing count by" + amount)
    (model.copy(count = model.count + amount), Logger.consoleLog[IO]("Changing count by " + amount))
  }
  // view triggered whenever model changes
  override def view(model: Model): Html[Msg] =
    div(
      button(onClick(Increament(1)))("Increase"),
      button(onClick(Increament(-1)))("Decrease"),
      div(s"Tyrian running: $model")
    )
}
