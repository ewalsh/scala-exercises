package ai.economicdatasciences.examples

import scala.annotation.tailrec

class TreasureHunters {
  // first split into array of ints and order
  def splitStrAndOrder(str: String): Array[Int] = {
    val arr = str.split(",").map(_.toInt)
    arr.sorted
  }
  // given an array of the trasure bags, identify the lowest value or default 0
  def findLowest(arr: Array[Int]): Int = {
    val tmpMin = arr.min
    arr.indexOf(tmpMin)
  }
  // take in the ordered array and add treasure to n hunters stored in array
  def splitTreasure(str: String, n: Int): Array[Int] = {
    val treasureArr = splitStrAndOrder(str)
    val treasureBags: Array[Int] = Array.fill(n)(0)
    val startIter = treasureArr.length - 1
    @tailrec
    def addTreasure(iter: Int, treasure: Array[Int], bags: Array[Int]): Array[Int] = {
      val id = findLowest(bags)
      iter match {
        case 0 => {
          bags(id) += treasure(iter)
          bags
        }
        case _ => {
          bags(id) += treasure(iter)
          addTreasure(iter - 1, treasure, bags)
        }
      }
    }
    addTreasure(startIter, treasureArr, treasureBags)
  }

  // check if all are equal
  def checkAllEqual(arr: Array[Int]): Boolean = {
    val firstElem = arr(0)
    val checkSum: Int = arr.map(x => {
      (x == firstElem) match {
        case true => 0
        case false => 1
      }
    }).sum
    checkSum match {
      case 0 => true
      case _ => false
    }
  }
}


object SplitTreasureApp {
  def main(args: Array[String]): Unit = {
    val str = args(0)
    val n = args(1).toInt
    val treasureHunters = new TreasureHunters
    val treasureBags = treasureHunters.splitTreasure(str, n)
    val isSplittable = treasureHunters.checkAllEqual(treasureBags)
    isSplittable match {
      case true => {
        treasureBags.zipWithIndex.map(tup => {
          println(s"Treasure hunder ${tup._2} receives ${tup._1}")
        })
      }
      case false => println("The Treasure cannot be split")
    }
  }
}
