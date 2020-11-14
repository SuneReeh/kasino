package kasino

trait SuitedCard extends Card {
  /**
   * @return The companion object of the class implementing SuitedCard
   */
  override val companion: SuitedCardCompanion[SuitedCard]

  /**
   * @return The suit of the card
   */
  def suit: companion.SuitTrait

  /**
   * @return The rank of the card
   */
  def rank: companion.RankTrait

  override def isSpades: Boolean = suit.isSpades

  override def isFiveOfSpades: Boolean = isSpades && (rank.ordinal() == 5)
}

/**
 * Trait required to be implemented by a companion object associated to any class implemeting SuitedCard.
 *
 * @tparam C The class implementing SuitedCard
 */
trait SuitedCardCompanion[+C <: SuitedCard] extends CardCompanion[C] {
  protected[kasino] trait SuitTrait extends java.lang.Enum[SuitTrait] with scala.reflect.Enum {
    def isSpades: Boolean
  }
  
  protected[kasino] trait RankTrait extends java.lang.Enum[RankTrait] with scala.reflect.Enum {
  }
}
