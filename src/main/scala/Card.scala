trait Card {
  /**
   * @return The companion object of the class implementing Card
   */
  def companion: CardCompanion[Card]
  
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

trait CardCompanion[+C <: Card] {
  def stringToCard(string: String): C
}
