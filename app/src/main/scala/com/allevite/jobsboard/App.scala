package com.allevite.jobsboard

import scala.scalajs.js.annotation.*
import org.scalajs.dom.document
@JSExportTopLevel("AlleViteApp")
class App {
  @JSExport
  def doSomething(containerId: String) =
    document.getElementById(containerId).innerHTML = "Alle vite"

  // in JS: document.getElementById(...) .innerHTML = "THIS IS MY HTML"
}
