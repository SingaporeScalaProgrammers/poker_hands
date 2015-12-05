package pokerhands

import scala.util.Try

sealed trait Suit
object Spades extends Suit
object Hearts extends Suit
object Clubs extends Suit
object Diamonds extends Suit

sealed trait Value extends Ordered[Value]{
  def rank : Int

  override def compare(that: Value): Int = rank - that.rank
}

object Two extends Value{
  override def rank: Int = 0
}
object Three extends Value {
  override def rank: Int = 1
}
object Four extends Value {
  override def rank: Int = 2
}
object Five extends Value {
  override def rank: Int = 3
}
object Six extends Value {
  override def rank: Int = 4
}
object Seven extends Value {
  override def rank: Int = 5
}
object Eight extends Value {
  override def rank: Int = 6
}
object Nine extends Value {
  override def rank: Int = 7
}
object Ten extends Value {
  override def rank: Int = 8
}
object Jack extends Value {
  override def rank: Int = 9
}
object Queen extends Value {
  override def rank: Int = 10
}
object King extends Value {
  override def rank: Int = 11
}
object Ace extends Value {
  override def rank: Int = 12
}

object Cards{
  val suits = List(Hearts,Diamonds,Spades,Clubs)
  val values = List(Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace)
  val pack = for {
    s <- suits
    v <- values
  }yield new Card(s,v)
}


case class Card(suit : Suit, value : Value) extends Ordered[Card]{
  override def compare(that: Card): Int = value.compare(that.value)
}

object Hand{
  def apply(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card) : Hand = {
    Try(StraightFlush(card1,card2,card3,card4,card5)).recover{
      case _ => FourOfAKind(card1,card2,card3,card4,card5)
    }.recover{
      case _ => HighCard(card1,card2,card3,card4,card5)

    }
  }.get
}

sealed trait Hand extends Ordered[Hand]{
  def card1 : Card
  def card2 : Card
  def card3 : Card
  def card4 : Card
  def card5 : Card

  val cards = List(card1,card2,card3,card4,card5).sorted
  val cardsBySuit : Map[Suit,List[Card]] = cards.groupBy(_.suit)
  val cardsByValues : Map[Value,List[Card]] = cards.groupBy(_.value)
  val highestCard = cards.last
  val cardsAreOfSameSuit : Boolean = cardsBySuit.size == 1
  val cardsHaveConsecutiveValues : Boolean = cards.sliding(2).forall(pair => pair(0).value.rank == pair(1).value.rank -1)
}

case class HighCard(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card) extends Hand {
  override def compare(that: Hand): Int = ???
}

case class StraightFlush(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card) extends Hand{

  require(cardsAreOfSameSuit && cardsHaveConsecutiveValues)

  override def compare(that: Hand): Int = that match{
    case h : StraightFlush => highestCard.value.rank - that.highestCard.value.rank
    case _ => 1
  }
}

case class FourOfAKind(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card) extends Hand {

  val fourCards: Map[Value, List[Card]] = cardsByValues.filter(_._2.length == 4)

  require(!fourCards.isEmpty)

  val fourGroupValue : Value = fourCards.values.head(0).value
  val singleGroupValue : Value = cardsByValues.filter(_._2.length == 1).values.head(0).value

  override def compare(that: Hand): Int = that match {
    case h : StraightFlush => -1
    case h : FourOfAKind => if(h.fourGroupValue.rank == fourGroupValue.rank) {
      singleGroupValue.rank - h.singleGroupValue.rank
    }else  fourGroupValue.rank  - h.fourGroupValue.rank
    case h : Hand => 1
  }
}

case class Straight(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card) extends Hand {

  require(cardsHaveConsecutiveValues)

  override def compare(that: Hand): Int = ???
}

case class Flush(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card) extends Hand {

  require(cardsAreOfSameSuit)

  override def compare(that: Hand): Int = ???
}



class PokerHands {


}
