package pokerhands


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
  def apply(fiveCards: FiveCards) : Hand = fiveCards match {
    case StraightFlush(v) => StraightFlush(v)
    case FourOfAKind(v) => FourOfAKind(v)
    case MatchesHighCard(v) => HighCard(v)
  }
}

sealed trait Hand extends Ordered[Hand]{
}

object MatchesHighCard extends CardOps{
  def unapply(cards  : FiveCards) : Option[Value] = Some(highestCard(cards.cards).value)
}

case class HighCard(highestValue : Value) extends Hand {
  override def compare(that: Hand): Int = that match{
    case HighCard(v) => highestValue.compare(v)
    case _ => -1
  }
}

trait CardOps{

  def cardsBySuit(cards : List[Card]) : Map[Suit,List[Card]] = cards.groupBy(_.suit)
  def cardsByValues(cards : List[Card]) : Map[Value,List[Card]] = cards.groupBy(_.value)
  def highestCard(cards : List[Card]) = cards.sorted.last
  def cardsAreOfSameSuit(cards : List[Card]) : Boolean = cardsBySuit(cards).size == 1
  def cardsHaveConsecutiveValues(cards : List[Card]) : Boolean = cards.sorted.sliding(2).forall(pair => pair(0).value.rank == pair(1).value.rank -1)

}

case class FiveCards(card1 : Card, card2: Card,card3 : Card,card4 : Card,card5 :Card){
  val cards = List(card1,card2,card3,card4,card5)
}

object StraightFlush extends CardOps{

  def unapply(cards : FiveCards) : Option[Value] = {
    if(cardsAreOfSameSuit(cards.cards) && cardsHaveConsecutiveValues(cards.cards)) {
      Some(highestCard(cards.cards).value)
    }else None
  }
}

case class StraightFlush(highestCardValue : Value) extends Hand{

  override def compare(that: Hand): Int = that match{
    case h : StraightFlush => highestCardValue.compare(h.highestCardValue)
    case _ => 1
  }
}

object FourOfAKind extends CardOps{

  def unapply(fiveCards: FiveCards): Option[Value] ={
    val fourCards: Map[Value, List[Card]] = cardsByValues(fiveCards.cards).filter(_._2.length == 4)
    if(!fourCards.isEmpty) Some(fourCards.values.head(0).value) else None
  }
}

case class FourOfAKind(fourGroupValue : Value) extends Hand {

  override def compare(that: Hand): Int = that match {
    case h : StraightFlush => -1
    case h : FourOfAKind => fourGroupValue.compare(h.fourGroupValue)
    case h : Hand => 1
  }
}




