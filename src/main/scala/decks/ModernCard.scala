package kasino.decks

import kasino.{SuitedCard, SuitedCardCompanion}

class ModernCard extends SuitedCard {
  /**
   * @return The companion object of the class implementing SuitedCard
   */
  override val companion: SuitedCardCompanion[ModernCard] = ModernCard

  /**
   * @return The suit of the card
   */
  override def suit: companion.SuitTrait = ???

  /**
   * @return The rank of the card
   */
  override def rank: companion.RankTrait = ???

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
  enum Suit extends SuitTrait {
    case Clubs, Diamonds, Hearts, Spades

    override def isSpades: Boolean = this == Spades
  } 
  
  override def stringToCard(string: String): ModernCard = ???
}
