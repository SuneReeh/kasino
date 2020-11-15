package kasino.cards

trait Card {
  /**
   * @return The companion object of the class implementing Card
   */
  val companion: CardCompanion[Card]
  
  /**
   * @return The set of possible values that the Card can be used as.
   */
  def values: Set[Int]

  /**
   * @return How many points the Card is worth.
   */
  def points: Int
  
  def isFiveOfSpades: Boolean
  
  def isSpades: Boolean
}

/**
 * Trait required to be implemented by a companion object associated to any class implemeting Card.
 * 
 * @tparam C The class implementing Card
 */
trait CardCompanion[+C <: Card] {
  def apply(string: String): C = stringToCard(string)
  
  def stringToCard(string: String): C
}
