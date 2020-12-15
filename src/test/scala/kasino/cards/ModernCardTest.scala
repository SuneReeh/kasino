package kasino.cards

import org.scalatest.prop.TableFor1
import kasino.UnitTestSpec
import kasino.cards.ModernCard
import kasino.cards.ModernCard.Rank
import kasino.cards.ModernCard.Rank._
import kasino.cards.ModernCard.Suit
import kasino.cards.ModernCard.Suit._


class ModernCardTest extends UnitTestSpec  {
  val clubs: TableFor1[ModernCard] = {
    Table("card") ++ (
      for (rank <- Rank.values if rank.ordinal > 0) yield ModernCard(Clubs, rank)
    )
  }
  val diamonds: TableFor1[ModernCard] = {
    Table("card") ++ (
      for (rank <- Rank.values if rank.ordinal > 0) yield ModernCard(Diamonds, rank)
      )
  }
  val hearts: TableFor1[ModernCard] = {
    Table("card") ++ (
      for (rank <- Rank.values if rank.ordinal > 0) yield ModernCard(Hearts, rank)
      )
  }
  val spades: TableFor1[ModernCard] = {
    Table("card") ++ (
      for (rank <- Rank.values if rank.ordinal > 0) yield ModernCard(Spades, rank)
      )
  }
  val jokers:TableFor1[ModernCard] = Table("card", ModernCard(Joker,Zero))
  
  val nonJokers:TableFor1[ModernCard] = clubs ++ diamonds ++ hearts ++ spades
  val nonSpades:TableFor1[ModernCard] = clubs ++ diamonds ++ hearts ++ jokers
  val cards:TableFor1[ModernCard] = nonJokers ++ jokers
  
  val invalidData = Table(
    ("suit", "rank"),
    (Clubs, Zero),
    (Diamonds, Zero),
    (Hearts, Zero),
    (Spades, Zero)
  ) ++ (for (rank <- Rank.values if rank.ordinal > 0) yield (Joker, rank))
  
  /*
  "A non-joker" should "have rank greater than Zero" in {
    forAll (nonJokers) {(card: ModernCard) =>
      card.rank.ordinal should be > 0
    }
  }
  
  "A joker" should "have rank Zero" in {
    forAll (jokers) { (card: ModernCard) =>
      card.rank should equal (Zero)
    }
  }
   */
}
