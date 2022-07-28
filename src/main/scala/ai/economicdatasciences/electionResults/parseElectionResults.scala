package ai.economicdatasciences.examples

import scala.util.{Try, Success, Failure}

sealed trait Party {
  val full_name = ""
  val code = ""
}
case object C extends Party {
  override val full_name = "Conservative Party"
  override val code = "C"
}
case object L extends Party {
  override val full_name = "Labour Party"
  override val code = "L"
}
case object UKIP extends Party {
  override val full_name = "UKIP"
  override val code = "UKIP"
}
case object LD extends Party {
  override val full_name = "Liberal Democrats"
  override val code = "LD"
}
case object G extends Party {
  override val full_name = "Green Party"
  override val code = "G"
}
case object Ind extends Party {
  override val full_name = "Independent"
  override val code = "Ind"
}
case object SNP extends Party {
  override val full_name = "SNP"
  override val code = "SNP"
}

case class ElectionResult(id: Int, constituency: String, results: Map[Option[Party], Try[Int]], good_quality: Boolean, comment: String)
case class ElectionResultLong(key: String, id: Int, constituency: String, party: Party, votes: Int, good_quality: Boolean, comment: String)


class ElectionResultsParser(file: String) {
  // store good and bad results
  var goodResults = Array[ElectionResultLong]()
  // var badResults = Array[ElectionResultLong]()
  // parse party code from string
  def parseParty(partyCode: String): Option[Party] = {
    partyCode match {
      case "C" => Some(C)
      case "L" => Some(L)
      case "UKIP" => Some(UKIP)
      case "LD" => Some(LD)
      case "G" => Some(G)
      case "Ind" => Some(Ind)
      case "SNP" => Some(SNP)
      case _ => None
    }
  }
  // handle int parse
  def parseInt(strInt: String): Try[Int] = {
    Try {
      strInt.toInt
    }
  }
  // concat messages
  def concatErrors(input: List[(Boolean, String)]): String = {
    var outputMessage = ""
    for(i <- Range(0, input.length)){
      if(!input(i)._1){
        outputMessage = outputMessage + input(i)._2 + "; "
      }
    }
    outputMessage
  }
  // parse a line
  def parseLine(id: Int, str: String): ElectionResult = {
    val line = str.split(",").map(_.trim)
    val constit = line(0)
    val resList = line.drop(1).sliding(2,2).toList.map(res => (parseParty(res(1)) -> parseInt(res(0))))
    val noDuplicates = (resList.map(_._1).length == resList.map(_._1).distinct.length)
    val testParty = !resList.map(_._1).exists(_ == None)
    val testVotes = !resList.map(_._2).exists(_ == Failure)
    val testMessage = List(
      (noDuplicates, "Results were duplicated for a party"),
      (testParty, "A party name doesn't match expected party results"),
      (testVotes, "Vote count doesn't look like a number")
    )
    ElectionResult(id, constit, resList.toMap, (noDuplicates && testParty && testVotes), concatErrors(testMessage))
  }

  // parse the whole file
  def parseResultsFile(str: String): Array[ElectionResult] = {
    val arr = str.split("\n").zipWithIndex
    arr.map(x => parseLine(x._2, x._1))
  }

  // split okay results from erroneour
  def splitResultsOnQuality(fullResults: Array[ElectionResult]): Map[String, Array[ElectionResult]] = {
    val goodRes = fullResults.filter(_.good_quality == true)
    val badRes = fullResults.filter(_.good_quality == false)
    Map(("okay" -> goodRes), ("error" -> badRes))
  }

  // aggregate results
  def getGoodResults(fileStr: String): Unit = {
    val fullRes = parseResultsFile(fileStr)
    val resMap = splitResultsOnQuality(fullRes)
    val goodRes = resMap("okay")
    val badRes = resMap("error")
    var counter = 0
    for(res <- goodRes){
      for(partyResKey <- res.results.keys.map(_.get).toList){
        goodResults = goodResults :+ ElectionResultLong(
          key=partyResKey.code + "_" + counter,
          id=counter,
          constituency=res.constituency,
          party=partyResKey,
          votes=res.results(Some(partyResKey)).get,
          good_quality=true,
          comment="okay"
        )
      }
    }
    println("results parsed and ready!")
  }

  def getResults: Array[ElectionResultLong] = {
    goodResults
  }

  getGoodResults(file)

}
