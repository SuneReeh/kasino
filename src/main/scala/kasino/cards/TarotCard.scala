package kasino.cards

case class TarotCard(val suit: TarotCard.Suit, val rank: TarotCard.Rank, val isAlsoZero: Boolean = false) extends SuitedCard {
  require(isAlsoZero <= (suit == TarotCard.Suit.Joker), "Only jokers can optionally have zero as additional value.")
  require((suit != TarotCard.Suit.Joker) <= (rank.ordinal >= 1 && rank.ordinal <= 14), "Non-jokers only rank from Ace to King.")


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
      case (s, TarotCard.Rank.Ace) if s != TarotCard.Suit.Joker => tempValues += 15
      case (TarotCard.Suit.Swords, TarotCard.Rank.Two) => tempValues += 16
      case (TarotCard.Suit.Coins, TarotCard.Rank.Ten) => tempValues += 17
      case (TarotCard.Suit.Swords, TarotCard.Rank.Ten) => tempValues += 21
      case (TarotCard.Suit.Joker, _) if isAlsoZero => tempValues += 0
      case _ =>
    }
    tempValues
  }

  /**
   * @return How many points the Card is worth.
   */
  override val points: Int = {
    (suit, rank) match {
      case (s, TarotCard.Rank.Ace) if s != TarotCard.Suit.Joker => 1
      case (TarotCard.Suit.Swords, TarotCard.Rank.Two) => 1
      case (TarotCard.Suit.Staves, TarotCard.Rank.Ten) => 1
      case (TarotCard.Suit.Coins, TarotCard.Rank.Ten) => 2
      case (TarotCard.Suit.Cups, TarotCard.Rank.Ten) => 3
      case (TarotCard.Suit.Swords, TarotCard.Rank.Ten) => 4
      case _ => 0
    }
  }

  override def toString: String = {
    if suit != TarotCard.Suit.Joker then return s"$rank of $suit"
    val name = rank.ordinal match {
      case 0 => "The Fool"
      case 1 => "The Magician"
      case 2 => "The High Priestess"
      case 3 => "The Empress"
      case 4 => "The Emperor"
      case 5 => "The Pope"
      case 6 => "The Lovers"
      case 7 => "The Chariot"
      case 8 => "Strength"
      case 9 => "The Hermit"
      case 10 => "The Wheel of Fortune"
      case 11 => "Justice"
      case 12 => "The Hanged Man"
      case 13 => "Death"
      case 14 => "Temperance"
      case 15 => "The Devil"
      case 16 => "The Tower"
      case 17 => "The Star"
      case 18 => "The Moon"
      case 19 => "The Sun"
      case 20 => "Judgement"
      case 21 => "The Universe"
    }
    name + (if isAlsoZero then " (0)" else "")
  }
}

object TarotCard extends SuitedCardCompanion[TarotCard] {
  override def apply(suit: Suit, rank: Rank): TarotCard = new TarotCard(suit, rank)

  def apply(suit: Suit, rank: Rank, isAlsoZero: Boolean = false): TarotCard = new TarotCard(suit, rank, isAlsoZero)

  def apply(string: String, isAlsoZero: Boolean): TarotCard = stringToCard(string, isAlsoZero)

  enum Suit extends java.lang.Enum[Suit] with SuitTrait {
    case Staves, Coins, Cups, Swords, Joker

    override

    def isSpades: Boolean = this == Swords
  }

  enum Rank extends java.lang.Enum[Rank] with RankTrait {
    case Zero, Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Page, Knight, Queen, King, Fifteen, Sixteen, Seventeen, Eighteen, Nineteen, Twenty, TwentyOne
  }

  override def stringToCard(string: String): TarotCard = stringToCard(string, false)
  
  def stringToCard(string: String, isAlsoZero: Boolean = false): TarotCard = {
    if string.contains(" of ") then
      val Array(rankString, suitString) = string.split(" of ", 2)
      return TarotCard(Suit.valueOf(suitString.toLowerCase.capitalize), Rank.valueOf(rankString.toLowerCase.capitalize))
    string.toLowerCase match {
      case "fool" | "the fool" => TarotCard(Suit.Joker, Rank.Zero, isAlsoZero)
      case "magician" | "the magician" => TarotCard(Suit.Joker, Rank.Ace, isAlsoZero)
      case "priestess" | "the priestess" | "high priestess" | "the high priestess" => TarotCard(Suit.Joker, Rank.Two, isAlsoZero)
      case "empress" | "the empress" => TarotCard(Suit.Joker, Rank.Three, isAlsoZero)
      case "emperor" | "the emperor" => TarotCard(Suit.Joker, Rank.Four, isAlsoZero)
      case "priest" | "the priest" | "pope" | "the pope" | "hierophant" | "the hierophant" => TarotCard(Suit.Joker, Rank.Five, isAlsoZero)
      case "lovers" | "the lovers" => TarotCard(Suit.Joker, Rank.Six, isAlsoZero)
      case "chariot" | "the chariot" => TarotCard(Suit.Joker, Rank.Seven, isAlsoZero)
      case "strength" => TarotCard(Suit.Joker, Rank.Eight, isAlsoZero)
      case "hermit" | "the hermit" => TarotCard(Suit.Joker, Rank.Nine, isAlsoZero)
      case "wheel of fortune" | "the wheel of fortune" => TarotCard(Suit.Joker, Rank.Ten, isAlsoZero)
      case "justice" => TarotCard(Suit.Joker, Rank.Page, isAlsoZero)
      case "hanged man" | "the hanged man" => TarotCard(Suit.Joker, Rank.Knight, isAlsoZero)
      case "death" => TarotCard(Suit.Joker, Rank.Queen, isAlsoZero)
      case "temperance" => TarotCard(Suit.Joker, Rank.King, isAlsoZero)
      case "devil" | "the devil" => TarotCard(Suit.Joker, Rank.Fifteen, isAlsoZero)
      case "tower" | "the tower" => TarotCard(Suit.Joker, Rank.Sixteen, isAlsoZero)
      case "star" | "the star" => TarotCard(Suit.Joker, Rank.Seventeen, isAlsoZero)
      case "moon" | "the moon" => TarotCard(Suit.Joker, Rank.Eighteen, isAlsoZero)
      case "sun" | "the sun" => TarotCard(Suit.Joker, Rank.Nineteen, isAlsoZero)
      case "judgement" => TarotCard(Suit.Joker, Rank.Twenty, isAlsoZero)
      case "world" | "the world" | "universe" | "the universe" => TarotCard(Suit.Joker, Rank.TwentyOne, isAlsoZero)
      case _ => throw new IllegalArgumentException("Not a valid TarotCard name: " + string)
    }
  }

  override def newDeck: Seq[TarotCard] = {
    newDeck(Set())
  }

  def newDeck(extraZeroJokers: Set[Int]): Seq[TarotCard] = {
    var deck: Seq[TarotCard] = for {
      suit <- Suit.values.toSeq if suit != Suit.Joker
      rank <- Rank.values.toSeq if (1 <= rank.ordinal) && (rank.ordinal <= 14)
    } yield TarotCard(suit, rank)
    var jokers: Seq[TarotCard] = for {
      rank <- Rank.values.toSeq
    } yield TarotCard(Suit.Joker, rank, extraZeroJokers.contains(rank.ordinal))
    deck ++ jokers
  }
}
