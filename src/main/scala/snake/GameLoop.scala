package snake

import scala.scalajs.js
import js.Dynamic.{ global => g }

class GameLoop {
  var then = js.Date.now()
  def loop(update: (js.Number) => Unit, render: () => Unit) = () => {
    val now = js.Date.now()
    val delta = now - then

    update(delta / 1000)
    render()

    then = now
  }

  def start(update: (js.Number) => Unit, render: () => Unit) = {
    g.setInterval(loop(update, render), 1)
  }
}