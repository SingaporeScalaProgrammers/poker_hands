package pokerhands
import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks

class PokerSpec extends FlatSpec with Matchers with PropertyChecks{

  val deals  = Gen.pick(5, Cards.pack)

  val cards = Gen.oneOf(Cards.pack)


  val hands = deals.map(cards => Hand(FiveCards(cards(0),cards(1), cards(2), cards(3), cards(4))))

  val threeHands = for{
    h1 <- hands
    h2 <- hands
    h3 <- hands
  }yield ((h1,h2,h3))

  "All poker Hands" should "be equal to themselves" in {
    forAll(hands)(hand => {
      hand should equal(hand)
    })
  }
  "Hand ordering" should "be commutative? ie if a < b and b < c, a < c" in {
    forAll(threeHands){
      th => {
        if((th._1 < th._2) && (th._2 < th._3)){
          (th._1 < th._3) should equal(true)
        }
      }
    }
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
    val strFlush = StraightFlush(Ten)
    forAll(hands){
        case h : StraightFlush =>  // there is a separate test for this
        case h : Hand => strFlush should be > h
      }
  }
  "Straight" should "be constructable from the correct cards" in {
    Straight(Card(Clubs,Two),Card(Hearts,Three), Card(Diamonds,Four), Card(Clubs,Five), Card(Hearts,Six))
  }
  "Flush" should "be constructable from the correct cards" in {
    Flush(Card(Clubs,Two),Card(Clubs,Four), Card(Clubs,Eight), Card(Clubs,King), Card(Clubs,Queen))
  }
  "Two straight flushes" should "be ranked according to the highest card" in {
    val lowSf = StraightFlush(Three)
    //interestingly if we leave the type hint off here we get a compile error
    val higherSf : Hand = StraightFlush(Four)
    lowSf should be < higherSf
  }
  "FourOfAKind" should "be higher than any hand that is not a StraightFlush" in {
    val foc : Hand = FourOfAKind(Two,Five)
    forAll(hands){
      case h : StraightFlush => (foc < h) should equal(true) //get a bizarre diverging implicit expansion error here if we write it another way
      case h : FourOfAKind => //TODO
      case h : Hand => foc should be > h
    }
  }
  "Two FourOfAKinds" should "be ranked by the value of the 4 cards and then by the value of the highest card" in {
    val foc : Hand = FourOfAKind(Two,Five)
    val foc2 : Hand = FourOfAKind(Three,Five)
    foc2 > foc should equal(true)

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
