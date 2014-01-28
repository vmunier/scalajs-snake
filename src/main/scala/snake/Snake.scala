package snake
import scala.scalajs.js

case class Snake(head: Block, tail: List[Block] = List()) {
  val speed: js.Number = 16 // movement in number of blocks per second
  val blocks = head +: tail

  def moveTailForward(): List[Block] = {
    for {
      (newTailPos, tailBlock) <- blocks.init.map(_.pos) zip tail
    } yield {
      tailBlock.copy(pos = newTailPos)
    }
  }
}
