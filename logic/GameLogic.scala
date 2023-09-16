package snake
import scala.scalajs.js
import js.Dynamic.{ global => g }
import org.scalajs.dom
import org.scalajs.dom.extensions._
import org.scalajs.dom.HTMLCanvasElement

sealed trait CellType
case object SnakeBody extends CellType
case object Apple extends CellType
case object Empty extends CellType

case class Cell(position: (Int, Int), cellType: CellType)

object SnakeGameLogic {
  // Constants
  val SCREEN_WIDTH = 800
  val SCREEN_HEIGHT = 600
  val SNAKE_SIZE = 20
  val SNAKE_COLOR = "rgb(0, 255, 0)"
  val BG_COLOR = "rgb(0, 4, 0)"
  var gameOver = false

  // Draw the snake head with an indicator arrow
  def drawSnakeHead(ctx: dom.CanvasRenderingContext2D): Unit = {
    val (headX, headY) = snakeSegments.head
    ctx.fillStyle = SNAKE_COLOR
    ctx.fillRect(headX, headY, SNAKE_SIZE, SNAKE_SIZE)

    // Draw the indicator arrow based on the snake's current direction
    ctx.fillStyle = "yellow"
    ctx.beginPath()
    ctx.moveTo(headX + SNAKE_SIZE / 2, headY + SNAKE_SIZE / 2)

    snakeDirection match {
      case "up" => ctx.lineTo(headX + SNAKE_SIZE / 2, headY)
      case "down" => ctx.lineTo(headX + SNAKE_SIZE / 2, headY + SNAKE_SIZE)
      case "left" => ctx.lineTo(headX, headY + SNAKE_SIZE / 2)
      case "right" => ctx.lineTo(headX + SNAKE_SIZE, headY + SNAKE_SIZE / 2)
    }

    ctx.closePath()
    ctx.fill()
  }

  // Initialize Snake
  var snakeSegments: List[(Int, Int)] = List((5 * SNAKE_SIZE, 3 * SNAKE_SIZE))
  var snakeDirection = "right"
  var snakeGrowth = 0

  // Function to generate a random position for the apple
  def generateApplePosition(): (Int, Int) = {
    var x, y = 0
    do {
      x = Random.nextInt(SCREEN_WIDTH / SNAKE_SIZE) * SNAKE_SIZE
      y = Random.nextInt(SCREEN_HEIGHT / SNAKE_SIZE) * SNAKE_SIZE
    } while (snakeSegments.contains((x, y))) // Ensure the apple doesn't overlap with the snake
    (x, y)
  }

  // Function to check for collisions
  def checkCollisions(): Boolean = {
    val head = snakeSegments.head
    // Check for collisions with boundaries
    if (head._1 < 0 || head._1 >= SCREEN_WIDTH || head._2 < 0 || head._2 >= SCREEN_HEIGHT) {
      return true
    }
    // Check for self-collision
    if (snakeSegments.tail.contains(head)) {
      return true
    }
    false
  }

  // Function to update the game state
  def updateGameState(): Unit = {
    // Move the Snake
    moveSnake()

    // Check if the snake hits the screen boundaries
    if (snakeSegments.head._1 < 0) {
      snakeSegments = snakeSegments.updated(0, (SCREEN_WIDTH - SNAKE_SIZE, snakeSegments.head._2))
    } else if (snakeSegments.head._1 >= SCREEN_WIDTH) {
      snakeSegments = snakeSegments.updated(0, (0, snakeSegments.head._2))
    }
    if (snakeSegments.head._2 < 0) {
      snakeSegments = snakeSegments.updated(0, (snakeSegments.head._1, SCREEN_HEIGHT - SNAKE_SIZE))
    } else if (snakeSegments.head._2 >= SCREEN_HEIGHT) {
      snakeSegments = snakeSegments.updated(0, (snakeSegments.head._1, 0))
    }

    // Keep the snake length equal to or less than segments
    while (snakeSegments.size > 3) {
      snakeSegments = snakeSegments.dropRight(1)
    }

    // Add segments to the snake's body if the snake has grown
    while (snakeGrowth > 0) {
      snakeSegments = snakeSegments :+ snakeSegments.last
      snakeGrowth -= 1
    }
  }

  // Function to reset the game (for game over)
  def resetGame(): Unit = {
    snakeSegments = List((5 * SNAKE_SIZE, 3 * SNAKE_SIZE))
    snakeDirection = "right"
    snakeGrowth = 0
    applePosition = generateApplePosition()
    gameOver = false
  }

  // Function to change the snake's direction
  def changeDirection(newDirection: String): Unit = {
    // Ensure that the snake cannot reverse its direction instantly
    if ((newDirection == "up" && snakeDirection != "down") ||
      (newDirection == "down" && snakeDirection != "up") ||
      (newDirection == "left" && snakeDirection != "right") ||
      (newDirection == "right" && snakeDirection != "left")) {
      snakeDirection = newDirection
    }
  }

  // Initialize cells
  val canvasWidth = SCREEN_WIDTH
  val canvasHeight = SCREEN_HEIGHT
  val cellSize = SNAKE_SIZE

  val cells: List[Cell] = {
    val positions = for {
      x <- 0 until canvasWidth by cellSize
      y <- 0 until canvasHeight by cellSize
    } yield (x, y)

    positions.map(position => Cell(position, Empty)).toList
  }

  // Function to create an ASCII representation of the game board
  def createAsciiBoard(snakeSegments: List[(Int, Int)], applePosition: (Int, Int), canvasWidth: Int, canvasHeight: Int, snakeSize: Int): String = {
    val columns = canvasWidth / snakeSize
    val rows = canvasHeight / snakeSize
    val board = Array.ofDim[Char](rows, columns)

    // Initialize the board with empty cells
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        board(row)(col) = '.'
      }
    }

    // Place the snake on the board
    for ((x, y) <- snakeSegments) {
      board(y / snakeSize)(x / snakeSize) = 'O'
    }

    // Place the apple on the board
    val (appleX, appleY) = applePosition
    board(appleY / snakeSize)(appleX / snakeSize) = 'A'

    // Create the ASCII representation
    val asciiBoard = new StringBuilder()
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        asciiBoard.append(board(row)(col))
      }
      asciiBoard.append('\n')
    }

    asciiBoard.toString()
  }
}

object SnakeGame {
  def main(args: Array[String]): Unit = {
    val canvas = dom.document.getElementById("gameCanvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // Function to display "Game Over" message and restart button
    def showGameOverScreen(): Unit = {
      ctx.fillStyle = "white"
      ctx.font = "48px sans-serif"
      ctx.textAlign = "center"
      ctx.fillText("Game Over", SnakeGameLogic.SCREEN_WIDTH / 2, SnakeGameLogic.SCREEN_HEIGHT / 2 - 48)

      // Create a restart button
      val restartButton = dom.document.createElement("button").asInstanceOf[dom.html.Button]
      restartButton.textContent = "Restart Game"
      restartButton.style.fontSize = "24px"
      restartButton.style.position = "absolute"
      restartButton.style.left = s"${SnakeGameLogic.SCREEN_WIDTH / 2 - 100}px"
      restartButton.style.top = s"${SnakeGameLogic.SCREEN_HEIGHT / 2}px"

      // Add a click event listener to restart the game when the button is clicked
      restartButton.addEventListener("click", (_: dom.MouseEvent) => {
        SnakeGameLogic.resetGame()
        restartButton.parentNode.removeChild(restartButton)
        // Start the game loop again
        gameLoop()
      })

      // Append the button to the body
      dom.document.body.appendChild(restartButton)
    }

    // Handle keyboard events
    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case KeyCode.Up => SnakeGameLogic.changeDirection("up")
        case KeyCode.Down => SnakeGameLogic.changeDirection("down")
        case KeyCode.Left => SnakeGameLogic.changeDirection("left")
        case KeyCode.Right => SnakeGameLogic.changeDirection("right")
        case _ =>
      }
    })

    // Game loop
    def gameLoop(): Unit = {
      SnakeGameLogic.updateGameState()

      // Check for collisions
      if (SnakeGameLogic.checkCollisions()) {
        // Handle game over
        dom.window.alert("Game Over")
        SnakeGameLogic.resetGame()
        return
      }

      // Check if the snake eats the apple
      val applePosition = SnakeGameLogic.applePosition
      if (SnakeGameLogic.snakeSegments.head == applePosition) {
        // Increase snake size and generate a new apple
        SnakeGameLogic.snakeSegments = SnakeGameLogic.snakeSegments :+ SnakeGameLogic.snakeSegments.last
        SnakeGameLogic.applePosition = SnakeGameLogic.generateApplePosition()
      }

      // Clear the canvas
      ctx.fillStyle = SnakeGameLogic.BG_COLOR
      ctx.fillRect(0, 0, SnakeGameLogic.SCREEN_WIDTH, SnakeGameLogic.SCREEN_HEIGHT)

      // Draw the game based on cells
      for (cell <- SnakeGameLogic.cells) {
        ctx.fillStyle = cell.cellType match {
          case SnakeBody => SnakeGameLogic.SNAKE_COLOR
          case Apple => "red"
          case Empty => SnakeGameLogic.BG_COLOR
        }
        ctx.fillRect(cell.position._1, cell.position._2, SnakeGameLogic.SNAKE_SIZE, SnakeGameLogic.SNAKE_SIZE)
      }

      // Draw the snake head and indicator arrow
      SnakeGameLogic.drawSnakeHead(ctx)

      // Check if the game is over
      if (SnakeGameLogic.gameOver) {
        showGameOverScreen()
        return // Stop the game loop
      }

      // Call the game loop recursively
      dom.window.requestAnimationFrame(_ => gameLoop())
    }

    // Start the game loop
    gameLoop()
  }
}
