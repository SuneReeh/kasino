package kasino.cards

case class ModernCard (val suit: ModernCard.Suit, val rank: ModernCard.Rank) extends SuitedCard {
  /**
   * @return The companion object of the class implementing SuitedCard
   */
  override val companion: ModernCard.type = ModernCard

  /**
   * @return The set of possible values that the Card can be used as.
   */
  override val values: Set[Int] = {
    var tempValues = Set[Int](rank.ordinal)
    (suit, rank) match {
      case (_, ModernCard.Rank.Ace) => tempValues += 14
      case (ModernCard.Suit.Spades, ModernCard.Rank.Two) => tempValues += 15
      case (ModernCard.Suit.Diamonds, ModernCard.Rank.Ten) => tempValues += 16
      case (ModernCard.Suit.Spades, ModernCard.Rank.Ten) => tempValues += 21
      case _ =>
    }
    tempValues
  }

  /**
   * @return How many points the Card is worth.
   */
  override val points: Int = {
    (suit, rank) match {
      case (_, ModernCard.Rank.Ace) => 1
      case (ModernCard.Suit.Spades, ModernCard.Rank.Two) => 1
      case (ModernCard.Suit.Clubs, ModernCard.Rank.Ten) => 1
      case (ModernCard.Suit.Diamonds, ModernCard.Rank.Ten) => 2
      case (ModernCard.Suit.Hearts, ModernCard.Rank.Ten) => 3
      case (ModernCard.Suit.Spades, ModernCard.Rank.Ten) => 4
      case _ => 0
    }
  }

  override def toString: String = if suit == ModernCard.Suit.Joker then "Joker" else s"$rank of $suit"
}

object ModernCard extends SuitedCardCompanion[ModernCard] {
  override def apply(suit: Suit, rank: Rank): ModernCard = new ModernCard(suit,rank)

  enum Suit extends java.lang.Enum[Suit] with SuitTrait 
  {
    case Clubs, Diamonds, Hearts, Spades, Joker

    override def isSpades: Boolean = this == Spades
  }
  
  enum Rank extends java.lang.Enum[Rank] with RankTrait
  {
    case Zero, Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King
  }
  
  override def stringToCard(string: String): ModernCard = {
    if string.strip().toLowerCase == "joker" then
      return ModernCard(Suit.Joker, Rank.Zero)
    val Array(rankString,suitString) = string.split(" of ", 2)
    ModernCard(Suit.valueOf(suitString.toLowerCase.capitalize), Rank.valueOf(rankString.toLowerCase.capitalize))
  }
}
