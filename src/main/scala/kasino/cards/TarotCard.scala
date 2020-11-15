package kasino.cards

case class TarotCard(val suit: TarotCard.Suit, val rank: TarotCard.Rank) extends SuitedCard {
  /**
   * @return The companion object of TarotCard
   */
  override val companion: TarotCard.type = TarotCard

  /**
   * @return The set of possible values that the Card can be used as.
   */
  override val values: Set[Int] = {
    var tempValues = Set[Int](rank.ordinal)
    (suit, rank) match {
      case (_, TarotCard.Rank.Ace) => tempValues += 14
      case (TarotCard.Suit.Swords, TarotCard.Rank.Two) => tempValues += 15
      case (TarotCard.Suit.Coins, TarotCard.Rank.Ten) => tempValues += 16
      case (TarotCard.Suit.Swords, TarotCard.Rank.Ten) => tempValues += 21
      case _ =>
    }
    tempValues
  }

  /**
   * @return How many points the Card is worth.
   */
  override val points: Int = {
    (suit, rank) match {
      case (_, TarotCard.Rank.Ace) => 1
      case (TarotCard.Suit.Swords, TarotCard.Rank.Two) => 1
      case (TarotCard.Suit.Staves, TarotCard.Rank.Ten) => 1
      case (TarotCard.Suit.Coins, TarotCard.Rank.Ten) => 2
      case (TarotCard.Suit.Cups, TarotCard.Rank.Ten) => 3
      case (TarotCard.Suit.Swords, TarotCard.Rank.Ten) => 4
      case _ => 0
    }
  }

  override def toString: String = if suit == TarotCard.Suit.Joker then "Joker" else s"$rank of $suit"
}

object TarotCard extends SuitedCardCompanion[TarotCard] {
  override def apply(suit: Suit, rank: Rank): TarotCard = new TarotCard(suit,rank)

  enum Suit extends java.lang.Enum[Suit] with SuitTrait
  {
    case Staves, Coins, Cups, Swords, Joker

    override def isSpades: Boolean = this == Swords
  }

  enum Rank extends java.lang.Enum[Rank] with RankTrait
  {
    case Zero, Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Page, Knight, Queen, King, Fifteen, Sixteen, Seventeen, Eighteen, Nineteen, Twenty, TwentyOne
  }

  override def stringToCard(string: String): TarotCard = {
    if string.contains(" of ") then
      val Array(rankString,suitString) = string.split(" of ", 2)
      return TarotCard(Suit.valueOf(suitString.toLowerCase.capitalize), Rank.valueOf(rankString.toLowerCase.capitalize))
    string.toLowerCase match {
      case "fool" | "the fool" => TarotCard(Suit.Joker, Rank.Zero)
      case "magician" | "the magician" => TarotCard(Suit.Joker, Rank.Ace)
      case "priestess" | "the priestess" | "the high priestess" => TarotCard(Suit.Joker, Rank.Two)
      case "empress" | "the empress" => TarotCard(Suit.Joker, Rank.Three)
      case "emperor" | "the emperor" => TarotCard(Suit.Joker, Rank.Four)
      case "priest" | "the priest" | "pope" | "the pope" | "hierophant" | "the hierophant" => TarotCard(Suit.Joker, Rank.Five)
      case "lovers" | "the lovers" => TarotCard(Suit.Joker, Rank.Six)
      case "chariot" | "the chariot" => TarotCard(Suit.Joker, Rank.Seven)
      case "strength" => TarotCard(Suit.Joker, Rank.Eight)
      case "hermit" | "the hermit" => TarotCard(Suit.Joker, Rank.Nine)
      case "wheel of fortune" | "the wheel of fortune" => TarotCard(Suit.Joker, Rank.Ten)
      case "justice" => TarotCard(Suit.Joker, Rank.Page)
      case "hanged man" | "the hanged man" => TarotCard(Suit.Joker, Rank.Knight)
      case "death" => TarotCard(Suit.Joker, Rank.Queen)
      case "temperance" => TarotCard(Suit.Joker, Rank.King)
      case "devil" | "the devil" => TarotCard(Suit.Joker, Rank.Fifteen)
      case "tower" | "the tower" => TarotCard(Suit.Joker, Rank.Sixteen)
      case "star" | "the star" => TarotCard(Suit.Joker, Rank.Seventeen)
      case "moon" | "the moon" => TarotCard(Suit.Joker, Rank.Eighteen)
      case "sun" | "the sun" => TarotCard(Suit.Joker, Rank.Nineteen)
      case "judgement" => TarotCard(Suit.Joker, Rank.Twenty)
      case "world" | "the world" | "universe" | "the universe" => TarotCard(Suit.Joker, Rank.TwentyOne)
      case _ => throw new IllegalArgumentException("Not a valid TarotCard name: " + string)
    }
  }

  override def newDeck: Seq[TarotCard] = {
    var deck: Seq[TarotCard] = for {
      suit <- Suit.values.toSeq if suit != Suit.Joker
      rank <- Rank.values.toSeq if (1 <= rank.ordinal) && (rank.ordinal <= 14)
    } yield TarotCard(suit,rank)
    var jokers: Seq[TarotCard ] = for {
      rank <- Rank.values.toSeq
    } yield TarotCard(Suit.Joker,rank)
    deck ++ jokers
  }
}
