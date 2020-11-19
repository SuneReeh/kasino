package kasino.cards

/**
 *  Trait for cards that can be used in a game of "Nørdekasino".
 */
trait Card {

  /**
   * @return The companion object of the class implementing [[Card]].
   */
  val companion: CardCompanion[Card]
  
  /**
   * @return The set of possible values that this [[Card]] can be used as.
   */
  def values: Set[Int]

  /**
   * @return How many points this [[Card]] is worth.
   */
  def points: Int

  /**
   * @return [[true]] if this [[Card]] is the Five of Spades (or equivalent card).
   */
  def isFiveOfSpades: Boolean

  /**
   * @return [[true]] if this [[Card]] is Spades (or equivalent suit).
   */
  def isSpades: Boolean
  
  /**
   * @return Optional flavor description of this [[Card]].
   */
  def description: Option[String] = None
}

/**
 * Trait required to be implemented by a companion object associated to any class implemeting [[Card]].
 * 
 * @tparam C a class implementing [[Card]]
 */
trait CardCompanion[+C <: Card] {
  /**
   * Creates a [[C]] from its name.
   * 
   * @param name the name of the [[C]] to create
   * @return a new [[C]] representing the card with the given name.
   */
  def apply(name: String): C = stringToCard(name)

  /**
   * Creates a [[C]] from its name.
   * 
   * @param name the name of the [[C]] to create
   * @return a new [[C]] representing the card with the given name.
   */
  def stringToCard(name: String): C
  
  /**
   * Creates a new default deck populated with cards of type [[C]]. There is no guarentee that the deck is randomized, so the user should shuffle the deck themself before playing.
   * 
   * @return a new default [[C]] deck
   */
  def newDeck: Seq[C]
}
