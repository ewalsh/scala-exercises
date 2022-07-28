package ai.economicdatasciences.examples

class Board(nrows: Int, ncols: Int) {
  var board = Array.fill(nrows, ncols)(".")
  val numToConnect: Int = 4
  var winningSymbol: Option[String] = None
  var isWinner = false

  def getWinnerBoolean: Boolean = {
    isWinner
  }

  def printBoard(): Unit = {
    for(i <- Range(0, board.length)){
      println(s"${i} " + board(i).map(_ + " ").mkString)
    }
    println("  " + Range(0, board(0).length).toList.map(_ + " ").mkString)
  }

  def bool2Int(str1: String, str2: String): Int = {
    (str1 == str2) match {
      case true => 1
      case false => 0
    }
  }

  def checkRowsAndCols(str: String): Boolean = {
    val startCount = 0
    val startIter = board.length - 1
    def goRow(row: Array[String], str: String): Boolean = {
      row.sliding(numToConnect,1).toList.map(_.map(x => {
        bool2Int(x, str)
      }).sum).exists(_ >= numToConnect)
    }
    val allRows = board.map(row => {
      goRow(row, str)
    })
    val allCols = board.transpose.map(row => {
      goRow(row, str)
    })
    (allRows ++ allCols).exists(x => x == true)
  }

  def checkDiags(str: String): Boolean = {
    val rowGroups = Range(0 , board.length).sliding(numToConnect, 1).toList
    val colGroupsRight = Range(0 , board(0).length).sliding(numToConnect, 1).toList
    val colGroupsLeft = colGroupsRight.map(_.map( x => board(0).length - x - 1))
    var booList = List[Boolean]()
    for(rg <- rowGroups){
        val tmpRows = board.slice(rg.min, rg.max + 1)
        for(cgr <- colGroupsRight){
          val tmpArr = tmpRows.transpose.slice(cgr.min, cgr.max + 1)
          val tmpIds = cgr.map(_ - cgr.min)
          val tmpDiag = tmpIds.zip(tmpArr).map(x => x._2(x._1))
          booList = booList :+ (tmpDiag.map(x => bool2Int(str, x)).sum == numToConnect)
        }
        for(cgl <- colGroupsLeft){
          val tmpArr = tmpRows.transpose.slice(cgl.min, cgl.max + 1)
          val tmpIds = cgl.map(_ - cgl.min)
          val tmpDiag = tmpIds.zip(tmpArr).map(x => x._2(x._1))
          booList = booList :+ (tmpDiag.map(x => bool2Int(str, x)).sum == numToConnect)
        }
    }
    booList.exists(_ == true)
  }

  def checkForWinner(): Unit = {
    val uniqueStr = board.flatten.distinct.filter(_ != ".")
    val rcWinners = uniqueStr.map(str => checkRowsAndCols(str))
    val diagWinners = uniqueStr.map(str => checkDiags(str))
    val winners = rcWinners.zip(diagWinners).map(x => (x._1 || x._2))
    winners.exists(_ == true) match {
      case false => println("sorry, no winner")
      case true => {
        isWinner = true
        val winAndLose = winners.zip(uniqueStr).groupBy(_._1)
        winningSymbol = Some(winAndLose(true).map(_._2).head)
        println(s"player ${winningSymbol.get} is the winner!")
      }
    }
  }

  def makeMove(col: Int, shape: String): Unit = {
    var transposedBoard = board.transpose
    val idBottom: Int = transposedBoard(col).map(x => {
      x match {
        case "." => 1
        case _ => 0
      }
    }).sum - 1
    (idBottom > -1) match {
      case true => {
        transposedBoard(col)(idBottom) = shape
        board = transposedBoard.transpose
      }
      case false => {
        println("that column is full, please make another move")
      }
    }

  }
}

object Connect4App {
  def main(args: Array[String]): Unit = {
    // collect arguments
    val nRows = args(0).toInt
    val nCols = args(1).toInt
    // create Board
    val b = new Board(nRows, nCols)
    var boardWinner = b.getWinnerBoolean
    // create list of columns to play
    val colList = Range(0, nCols).toList
    while(!boardWinner){
      b.makeMove(scala.util.Random.shuffle(colList).head, "x")
      b.checkForWinner
      boardWinner = b.getWinnerBoolean
      if(!boardWinner){
        b.makeMove(scala.util.Random.shuffle(colList).head, "o")
        b.checkForWinner
        boardWinner = b.getWinnerBoolean
      }
      b.printBoard
    }
  }
}
