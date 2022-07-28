package ai.economicdatasciences.examples

import scala.io.Source
import scala.util.Random

class Scrabble {
  val pointsTuples = List(
    (1, Array('E', 'A', 'I', 'O', 'N', 'R', 'T', 'L', 'S', 'U')),
    (2,	Array('D', 'G')),
    (3,	Array('B', 'C', 'M', 'P')),
    (4, Array('F', 'H', 'V', 'W', 'Y')),
    (5,	Array('K')),
    (8, Array('J', 'X')),
    (10, Array('Q', 'Z'))
  )

  val tileDistTuples = List(
    (12,Array('E')),
    (9,Array('A', 'I')),
    (8,Array('O')),
    (6,Array('N', 'R', 'T')),
    (4,Array('L', 'S', 'U', 'D')),
    (3,Array('G')),
    (2,Array('B', 'C', 'M', 'P', 'F', 'H', 'V', 'W', 'Y')),
    (1,Array('K', 'J', 'X', 'Q', 'Z'))
  )

  val dictionary = Source.fromFile("./data/dictionary.txt").getLines.toList.map(_.toUpperCase)
  val wordsByLength = dictionary.groupBy(_.length)
  val maxWordLength = wordsByLength.keys.max
  val allWordLengths = wordsByLength.keys.toArray.sorted

  var letterPool = Random.shuffle(tileDistTuples.map(letterDist => {
    letterDist._2.map(letter => Array.fill(letterDist._1)(letter)).flatten
  }).flatten)

  def assignLettersToRack: List[Char] = {
    val assignedLetters = letterPool.slice(0,7)
    letterPool = letterPool.drop(7)
    letterPool
  }

  def attemptWord(rack: List[Char], word: String): Option[String] = {
    val check = word.map(l => rack.exists(_ == l)).exists(_ == false)
    check match {
      case false => Some(word)
      case true => None
    }
  }

  def findLongestWord(rack: List[Char]): Option[String] = {
      var foundWord: Option[String] = None
      var counter = allWordLengths.length - 1
      while((foundWord == None) && (counter > -1)){
      val tmpWords = wordsByLength(allWordLengths(counter))
      val attemptedWordsList = tmpWords.map(w => attemptWord(rack, w))
      val ignore = attemptedWordsList.exists(_ != None) match {
        case true => {
          foundWord = attemptedWordsList.filter(_ == None).head
          foundWord
        }
        case false => None
      }
      counter = counter - 1
    }
    foundWord
  }
}
