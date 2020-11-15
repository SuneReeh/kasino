package kasino.cards

trait SuitedCard extends Card {
  /**
   * @return The companion object of the class implementing SuitedCard
   */
  override val companion: SuitedCardCompanion[SuitedCard]

  /**
   * @return The suit of the card
   */
  def suit: companion.Suit

  /**
   * @return The rank of the card
   */
  def rank: companion.Rank

  override def isSpades: Boolean = suit.isSpades

  override def isFiveOfSpades: Boolean = isSpades && (rank.ordinal == 5)
}

/**
 * Trait required to be implemented by a companion object associated to any class implemeting SuitedCard.
 *
 * @tparam C The class implementing SuitedCard
 */
trait SuitedCardCompanion[+C <: SuitedCard] extends CardCompanion[C] {
  type Suit <: SuitTrait
  type Rank <: RankTrait

  def apply(suit: Suit, rank: Rank): C
}

private trait SuitTrait extends scala.reflect.Enum {
  def isSpades: Boolean
}

private trait RankTrait extends scala.reflect.Enum {
}


