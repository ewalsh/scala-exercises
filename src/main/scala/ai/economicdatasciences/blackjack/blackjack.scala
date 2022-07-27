package ai.economicdatasciences.examples

import scala.util.Random
import scala.collection.mutable.ListBuffer

sealed trait Suit
case object Heart extends Suit
case object Diamond extends Suit
case object Spade extends Suit
case object Club extends Suit

sealed abstract class CardValue(val value: Int)
case object Ace extends CardValue(11)
case object King extends CardValue(10)
case object Queen extends CardValue(10)
case object Jack extends CardValue(10)
case object Ten extends CardValue(10)
case object Nine extends CardValue(9)
case object Eight extends CardValue(8)
case object Seven extends CardValue(7)
case object Six extends CardValue(6)
case object Five extends CardValue(5)
case object Four extends CardValue(4)
case object Three extends CardValue(3)
case object Two extends CardValue(2)

case class Card(suit: Suit, value: CardValue)

class Deck {
  // initialize deck
  def initDeck: List[Card] = {
    for {
      suit <- List(Heart, Diamond, Spade, Club)
      value <- List(Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two)
    } yield {
      new Card(suit, value)
    }
  }
  // shuffle deck
  def shuffleDeck: List[Card] = {
    Random.shuffle(initDeck)
  }

  // store the cards in the deck
  var cards: List[Card] = shuffleDeck

  // deal a card
  def dealCard: Option[Card] = {
    cards.length match {
      case 0 => None
      case _ => {
        val card = cards(0)
        cards = cards.slice(1, cards.length)
        Some(card)
      }
    }
  }
}

class Hand(name: String) {
  val cards = ListBuffer[Card]()
  val winningValue = 21

  // determin value of a hand
  def score: Int = cards.foldLeft(0)((result, card) => result + card.value.value)
  // add a card to the hand
  def addCard(c: Card): ListBuffer[Card] = {
    cards += c
  }
  // determine if is blackjack
  def isBlackjack: Boolean = {
    score  match {
      case `winningValue` => true
      case _ => false
    }
  }
  // determine if is bust
  def isBust: Boolean = {
    (score > winningValue)
  }
  // determin if will hit or stay
  def willHit(limit: Int, currentScore: Int): Boolean = {
    currentScore < limit
  }
}


class PlayBlackjack {
  var continuePlaying = true
  val gameSummary = ListBuffer[String]()
  var handCounter = 0
  val deck = new Deck
  val dealerHand = new Hand("Dealer")
  val samHand = new Hand("Sam")
  var foundWinner = false
  var tmpSummary = ""
  // initialize the hand
  dealerHand.addCard(deck.dealCard.get)
  dealerHand.addCard(deck.dealCard.get)
  samHand.addCard(deck.dealCard.get)
  samHand.addCard(deck.dealCard.get)

  while(continuePlaying){
    println()
    println("---------------- Starting New Hand ----------------")
    // check for winner by 21
    val dealerScore = dealerHand.score
    val samScore = samHand.score
    println(samScore)
    println(dealerScore)
    dealerScore match {
      case 21 => {
        samScore match {
          case 21 => {
            tmpSummary = s"Game is a tie, Dealer and Same have a score = ${dealerScore}"
            continuePlaying = false
          }
          case _ => {
            tmpSummary = s"Dealer wins with score = ${dealerScore}, Sam's score = ${samScore}"
            foundWinner = true
            continuePlaying = false
          }
        }
      }
      case _ => {
        samScore match {
          case 21 => {
            tmpSummary = s"Sam wins with score = ${samScore}, dealer's score = ${dealerScore}"
            foundWinner = true
            continuePlaying = false
          }
          case _ => {
            println("no one has 21")
          }
        }
      }
    }
  // check for bust
  (dealerScore > 21) match {
    case true => {
      tmpSummary = s"Sam won with a score = ${samScore} because the dealer went bust"
      foundWinner = true
      continuePlaying = false
    }
    case false => {
      (samScore > 21) match {
        case true => {
          tmpSummary = s"Dealer won with a score = ${dealerScore} because the sam went bust"
          foundWinner = true
          continuePlaying = false
        }
        case false => {
          println("no one has busted")
        }
      }
    }
  }

  (samScore > 16) match {
    case true => {
      dealerHand.addCard(deck.dealCard.get)
    }
    case false => {
      dealerHand.addCard(deck.dealCard.get)
      samHand.addCard(deck.dealCard.get)
    }
  }
  }
  println(tmpSummary)
}
