package ai.economicdatasciences.examples

class Grid {
  // initialize grid
  var grid = Array.fill(10, 10)(".")
  // initalize barrier
  val barrier = 9
  // initialize position
  var robotI = 9
  var robotJ = 0
  // initalize robot
  grid(robotI)(robotJ) = "x"

  // print grid
  def printGrid(): Unit = {
    for(i <- Range(0, grid.length)){
      println(s"${9 - i} " + grid(i).map(_ + " ").mkString)
    }
    println("  " + Range(0, grid(0).length).toList.map(_ + " ").mkString)
  }

  // update position
  def updatePosition(i: Int, j: Int): Unit = {
    grid = Array.fill(10, 10)(".")
    grid(i)(j) = "x"
  }

  // move robot south
  def moveWest(): Unit = {
    (robotJ - 1) match {
      case -1 => println("sorry I can't move any further west")
      case _ => {
        robotJ = robotJ - 1
        updatePosition(robotI, robotJ)
      }
    }
  }

  // move robot south
  def moveEast(): Unit = {
    (robotJ + 1) match {
      case 10 => println("sorry I can't move any further east")
      case _ => {
        robotJ = robotJ + 1
        updatePosition(robotI, robotJ)
      }
    }
  }

  // move robot south
  def moveSouth: Unit = {
    (robotI + 1) match {
      case 10 => println("sorry I can't move any further south")
      case _ => {
        robotI = robotI + 1
        updatePosition(robotI, robotJ)
      }
    }
  }

  // move robot south
  def moveNorth: Unit = {
    (robotI - 1) match {
      case -1 => println("sorry I can't move any further north")
      case _ => {
        robotI = robotI - 1
        updatePosition(robotI, robotJ)
      }
    }
  }

}

object RobotGridApp {
  def main(args: Array[String]): Unit = {
    val arrOfMoves = args(0).split(",")
    val g = new Grid
    for(move <- arrOfMoves){
      move match {
        case "N" => g.moveNorth
        case "S" => g.moveSouth
        case "E" => g.moveEast
        case "W" => g.moveWest
        case _ => println("sorry I don't recognize that direction")
      }
    }
    g.printGrid
  }
}
