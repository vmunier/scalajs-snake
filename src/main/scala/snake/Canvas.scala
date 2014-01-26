package snake

import scala.scalajs.js
import js.Dynamic.{ global => g }
import org.scalajs.dom
import org.scalajs.dom.extensions._

object Canvas {
  lazy val canvas = dom.document.createElement("canvas").cast[dom.HTMLCanvasElement]
  private lazy val ctx = canvas.getContext("2d").cast[dom.CanvasRenderingContext2D]

  // use a smaller inner window size value to prevent possible scrolling
  lazy val windowHeight = g.window.innerHeight * 0.98
  lazy val windowWidth = g.window.innerWidth * 0.98

  def init() = {
    canvas.width = Game.NbBlocksInWidth * Block.BlockSize
    canvas.height = windowHeight - (windowHeight % Block.BlockSize)
    g.document.body.appendChild(canvas)
    canvas
  }

  def render(snake: Snake, foods: Seq[Block], foodsInDigestion: Seq[Block], blocksEaten: Int) = {
    // clear window
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    renderBlocks(snake.blocks ++ foods ++ foodsInDigestion)
    displayScore(blocksEaten)

    if (Game.gameOver) {
      displayGameOver()
    }
  }

  private def displayScore(blocksEaten: Int) = {
    ctx.fillStyle = "black"
    ctx.font = "20px Arial"
    ctx.textAlign = "left"
    ctx.textBaseline = "top"
    ctx.fillText("Blocks eaten: " + blocksEaten, 32, 32)
  }

  private def displayGameOver() = {
    val (color, text) = if (Game.win) {
      ("green", "YOU WIN")
    } else {
      ("red", "GAME OVER")
    }
    ctx.fillStyle = color
    ctx.font = "60px Arial"
    ctx.textAlign = "center"
    ctx.textBaseline = "bottom"
    ctx.fillText(text, canvas.width / 2, canvas.height / 2)
  }

  private def renderBlocks(blocks: Seq[Block]) = {
    for (block <- blocks) {
      ctx.fillStyle = block.style
      ctx.fillRect(block.pos.x, block.pos.y, Block.BlockSize, Block.BlockSize)
    }
  }

}