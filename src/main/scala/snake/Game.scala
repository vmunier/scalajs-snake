package snake

import scala.scalajs.js
import js.Dynamic.{ global => g }
import org.scalajs.dom
import org.scalajs.dom.extensions._
import org.scalajs.dom.HTMLCanvasElement

trait GameVars {
  val NbBlocksInWidth = 10
  val FoodPeriodApparition = 15
  val MaxFoodAtSameTime = 3

  val canvas = Canvas.init()

  val blockPositions =
    for {
      x <- 0 until (canvas.width.toInt - Block.BlockSize) by Block.BlockSize
      y <- 0 until (canvas.height.toInt - Block.BlockSize) by Block.BlockSize
    } yield {
      Position(x, y)
    }

  var snake = Snake(
    head = Block(Position(0, 0), Colors.nextColor.rgb))

  var moveDoneInLastSecond = false
  var blocksEaten = 0
  var move: Move = Right
  var gameOver = false
  var win = false
}

object Game extends GameVars {
  Keyboard.init()

  def updateMove(): Unit = {
    val noTail = snake.tail.isEmpty
    val prevMove = move
    move =
      if (Keyboard.isHoldingLeft && (move != Right || noTail)) Left
      else if (Keyboard.isHoldingRight && (move != Left || noTail)) Right
      else if (Keyboard.isHoldingUp && (move != Down || noTail)) Up
      else if (Keyboard.isHoldingDown && (move != Up || noTail)) Down
      else move

    if (prevMove != move) {
      moveDoneInLastSecond = true
    }
  }

  var secondsAcc: js.Number = 0

  def update(seconds: js.Number) = {
    if (gameOver) {
      // do nothing
    } else if (blocksEaten >= (blockPositions.size / 2)) {
      //} else if (Food.availablePositions.isEmpty) {
      gameOver = true
      win = true
    } else {
      if (!moveDoneInLastSecond) {
        updateMove()
      }
      secondsAcc += seconds
      if (secondsAcc * snake.speed >= 1) {
        moveDoneInLastSecond = false
        disposeNewFood()
        moveSnake()
        handleCollisions()
        secondsAcc = 0
      }
    }
  }

  private def handleCollisions() = {
    val snakeBitesItsQueue = snake.tail.map(_.pos).contains(snake.head.pos)

    if (snakeBitesItsQueue) {
      gameOver = true
    } else {
      Food.foods.filter(food => food.pos == snake.head.pos).headOption.map { eatenFood =>
        Food.startDigestionForFood(eatenFood)
        blocksEaten += 1
      }

      Food.foodsInDigestion.filter(food => food.pos == snake.blocks.last.pos).headOption.map { foodReachingEndQueue =>
        snake = snake.copy(tail = snake.tail ++ Seq(foodReachingEndQueue))
        Food.endDigestionForFood(foodReachingEndQueue)
      }
    }
  }

  private def disposeNewFood() = {
    if (Food.foods.isEmpty ||
      (Food.foods.size < MaxFoodAtSameTime && js.Math.abs(Math.random() * FoodPeriodApparition).toInt == 0)) {
      Food.addRandomFood()
    }
  }

  def moveSnake() = {
    val modif = Block.BlockSize
    snake = snake.copy(tail = snake.moveTailForward())

    val headPos = snake.head.pos
    val newHeadPos = Position(
      x = (canvas.width + headPos.x + modif * horizontal) % canvas.width,
      y = (canvas.height + headPos.y + modif * vertical) % canvas.height)

    snake = snake.copy(head = snake.head.copy(pos = newHeadPos))
  }

  def horizontal() = move match {
    case Left => -1
    case Right => 1
    case _ => 0
  }

  def vertical() = move match {
    case Up => -1
    case Down => 1
    case _ => 0
  }

  def main(): Unit = {
    new GameLoop().start(update, () => Canvas.render(snake, Food.foods.toSeq, Food.foodsInDigestion.toSeq, blocksEaten))
  }
}
