package kasino.decks

import kasino.{SuitedCard, SuitedCardCompanion}

class ModernCard (val suit: ModernCard.Suit, val rank: ModernCard.Rank) extends SuitedCard {
  /**
   * @return The companion object of the class implementing SuitedCard
   */
  override val companion: ModernCard.type = ModernCard

  /**
   * @return The set of possible values that the Card can be used as.
   */
  override def values: Set[Int] = ???

  /**
   * @return How many points the Card is worth.
   */
  override def points: Int = ???
}

object ModernCard extends SuitedCardCompanion[ModernCard] {
  override def apply(suit: Suit, rank: Rank): ModernCard = new ModernCard(suit,rank)

  enum Suit extends SuitTrait {
    case Clubs, Diamonds, Hearts, Spades, Joker

    override def isSpades: Boolean = this == Spades
  }
  
  enum Rank extends RankTrait {
    case Zero, Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King
  }
  
  override def stringToCard(string: String): ModernCard = ???
}
