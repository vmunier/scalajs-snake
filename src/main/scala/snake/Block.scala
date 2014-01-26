package snake
import scala.scalajs.js
import js.Dynamic.{ global => g }

object Block {
  lazy val BlockSize = js.Math.floor(Canvas.windowWidth / Game.NbBlocksInWidth).toInt
}

case class Block(pos: Position, style: String)