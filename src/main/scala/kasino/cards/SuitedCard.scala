package kasino.cards

/**
 *  Specialized version of the trait [[Card]] intended for cards that have both a specified suit and specified rank.
 */
trait SuitedCard extends Card {

  /**
   * The companion object of the class implementing [[SuitedCard]].
   */
  override val companion: SuitedCardCompanion[SuitedCard]

  /**
   * The suit of the card
   */
  def suit: companion.Suit

  /**
   * The rank of the card
   */
  def rank: companion.Rank

  /**
   * [[true]] if this [[SuitedCard]] is Spades (or equivalent suit), as determined by [[suit.isSpades]].
   */
  override def isSpades: Boolean = suit.isSpades

  /**
   * [[true]] if this [[SuitedCard]] is the Five of Spades (or equivalent card), as determined by [[suit.isSpades]] and [[rank.ordinal]].
   */
  override def isFiveOfSpades: Boolean = isSpades && (rank.ordinal == 5)
}

/**
 * Trait required to be implemented by a companion object associated to any class implemeting [[SuitedCard]].
 *
 * @tparam C a class implementing [[SuitedCard]]
 */
trait SuitedCardCompanion[+C <: SuitedCard] extends CardCompanion[C] {
  /**
   * Enum type containing all possible suit values for cards of type [[C]].
   */
  type Suit <: SuitTrait
  /**
   * Enum type containing all possible rank values for cards of type [[C]]. From zero to however far the card type goes.
   */
  type Rank <: RankTrait

  /**
   * Creates a [[C]] with the specified [[Suit]] and [[Rank]].
   *
   * @param suit enum-value specifying the suit of the card.
   * @param rank enum-value specifying the rank of the card.
   * @return a new [[C]] with the specified [[Suit]] and [[Rank]]
   */
  def apply(suit: Suit, rank: Rank): C
}

/**
 * Trait implemented by enums specifying possible suits for a [[SuitedCard]].
 */
trait SuitTrait extends scala.reflect.Enum {
  /**
   * [[true]] if this suit is Spades (or equivalent).
   */
  def isSpades: Boolean
}

/**
 * Trait implemented by enums specifying possible ranks for a [[SuitedCard]].
 */
trait RankTrait extends scala.reflect.Enum {
}


