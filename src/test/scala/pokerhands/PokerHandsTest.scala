package pokerhands

import org.scalatest.{ Matchers, Spec }

import scala.util.Random

class PokerHandsTest extends Spec with Matchers {

  def `card values are ordered`(): Unit = {

    val ordered = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
    Random.shuffle(ordered).sorted should be(ordered)
  }

  def `cards are ordered`(): Unit = {

    val ordered = List(Two(♣), Three(♥), Four(♣), Five(♦), King(♠), Ace(♣))
    Random.shuffle(ordered).sorted should contain theSameElementsInOrderAs ordered
  }


  def `high card wins`() {

    val lowestHand = Hand(Seven(♠), Five(♣), Four(♦), Three(♦), Two(♣))
    val higherHand = Hand(Five(♣), Eight(♠), Four(♦), Three(♦), Two(♣))
    higherHand should be > lowestHand
  }

  def `pair wins`() {

    val lowestHand = Hand(Seven(♠), Five(♣), Four(♦), Three(♦), Two(♣))
    val higherHand = Hand(Five(♣), Five(♠), Four(♦), Three(♦), Two(♣))
    higherHand should be > lowestHand
  }
}
