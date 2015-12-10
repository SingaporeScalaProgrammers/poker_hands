package object pokerhands {

  sealed trait CardValue {

    def apply(s: Suit): Card = Card(s, this)
  }

  case object Two extends CardValue

  case object Three extends CardValue

  case object Four extends CardValue

  case object Five extends CardValue

  case object Six extends CardValue

  case object Seven extends CardValue

  case object Eight extends CardValue

  case object Nine extends CardValue

  case object Ten extends CardValue

  case object Jack extends CardValue

  case object Queen extends CardValue

  case object King extends CardValue

  case object Ace extends CardValue

  object CardValue {

    def order(v: CardValue): Int = v match {
      case Two => 2
      case Three => 3
      case Four => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Eight => 8
      case Nine => 9
      case Ten => 10
      case Jack => 11
      case Queen => 12
      case King => 13
      case Ace => 14
    }
  }

  implicit def cardValueOrdering[A <: CardValue]: Ordering[A] = Ordering.by(CardValue.order)

  sealed trait Suit

  case object ♣ extends Suit

  case object ♦ extends Suit

  case object ♥ extends Suit

  case object ♠ extends Suit

  case class Card(suit: Suit, value: CardValue)

  implicit def cardOrdering[A <: Card]: Ordering[A] = Ordering.by(_.value)

  case class Hand(cards: Card*) {

    def rankedCards: List[Card] = cards
      .sorted(cardOrdering.reverse)
      .toList
  }

  object Hand {
    def highCardOrdering[A <: Hand]: Ordering[A] = Ordering.by(_.rankedCards.toIterable)
  }

  case class Pair(value: CardValue, rest: Seq[Card])

  object Pair {

    def unapply(h: Hand): Option[Pair] = h.rankedCards
      .sliding(2)
      .collectFirst {
        case a :: b :: Nil if cardOrdering.equiv(a, b) =>
          Pair(a.value, h.cards.diff(Seq(a, b)))
      }

    val ordering: Ordering[Pair] = Ordering[(CardValue, Iterable[Card])].on(p => (p.value, p.rest))
  }


  implicit def handOrdering[A <: Hand]: Ordering[A] = new Ordering[A] {

    override def compare(x: A, y: A): Int = (x, y) match {
      case (Pair(xp), Pair(yp)) => Pair.ordering.compare(xp, yp)
      case (Pair(_), _) => 1
      case (_, Pair(_)) => -1
      case _ => Hand.highCardOrdering.compare(x, y)
    }
  }
}
