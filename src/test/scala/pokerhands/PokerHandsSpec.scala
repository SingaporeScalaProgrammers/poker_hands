package pokerhands
import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

class PokerSpec extends FlatSpec with Matchers with PropertyChecks{

  val deals  = Gen.pick(5, Card.pack)

  val cards = Gen.oneOf(Card.pack)

  val hands = deals.map(cards => Hand(cards(0),cards(1), cards(2), cards(3), cards(4)))

  "All poker Hands" should "be equal to themselves" in {
    forAll(hands)(hand => {
      hand should equal(hand)
    })
  }
  "Card values" should "be ranked correctly" in {
    //TODO how to test this
    pending
  }
  "A two" should "be lower than everything that is not a two and equal to other twos" in {
    val aTwo = Card(Spades,Two)
    forAll(cards){
      case other@Card(_,Two) => other.compare(aTwo) should equal(0)
      case other : Card => other should be > aTwo
    }
  }
  "Straight Flush" should "be higher than any other hand that is not a straight flush" in {
    val strFlush = StraightFlush(Card(Hearts,Two), Card(Hearts,Three), Card(Hearts,Four), Card(Hearts,Five), Card(Hearts, Six))
    forAll(hands){
        case h : StraightFlush =>  /// there is a separate test for this
        case h : Hand => strFlush should be > h
      }
  }
  it should "not be constructable with the wrong cards" in {
    intercept[IllegalArgumentException](StraightFlush(Card(Hearts,Two), Card(Hearts,Three), Card(Hearts,Four), Card(Hearts,Five), Card(Hearts, Two)))
  }
  "Straight" should "be constructable from the correct cards" in {
    Straight(Card(Clubs,Two),Card(Hearts,Three), Card(Diamonds,Four), Card(Clubs,Five), Card(Hearts,Six))
  }
  "Flush" should "be constructable from the correct cards" in {
    Flush(Card(Clubs,Two),Card(Clubs,Four), Card(Clubs,Eight), Card(Clubs,King), Card(Clubs,Queen))
  }
  "Two straight flushes" should "be ranked according to the highest card" in {
    val lowSf = StraightFlush(Card(Hearts,Two), Card(Hearts,Three), Card(Hearts,Four), Card(Hearts,Five), Card(Hearts, Six))
    //interestingly if we leave the type hint off here we get a compile error
    val higherSf : Hand = StraightFlush(Card(Hearts,Three), Card(Hearts,Four), Card(Hearts,Five), Card(Hearts, Six), Card(Hearts,Seven))
    lowSf should be < higherSf
  }

//  val cards : Gen[Card] = for{
//    s <- Gen.containerOf(Card.suits)
//    v <- Gen.oneOf(Card.values)
//  }yield Card(s,v)
//
//  "Cards" should "be unique" in {
//    var pack : Set[Card] = Set.empty
//
//    forAll(cards){ (card : Card) =>
//      //pack should not contain (card)
//      pack = pack + card
//      println(card)
//    }
//  }
}
