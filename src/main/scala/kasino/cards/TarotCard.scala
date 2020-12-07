package kasino.cards

/**
 * The classical tarot playing cards with four suits (staves, coins, cups, and swords) of ranks from ace to king (with page and knight replacing jacks from modern playing cards). In addition we have 22 jokers (the major arcana), all named, and numbered from 0 to 21.
 *
 * @constructor Create a new [[TarotCard]] with specified suit and rank.
 * @param suit the suit of this [[TarotCard]].
 * @param rank the rank of this [[TarotCard]].
 * @param isAlsoZero `true` if this [[TarotCard]] can also be used as value 0. Can only be `true` for jokers.
 * @note Jokers have the suit [[TarotCard.Suit.Joker]] and are the only cards with rank [[TarotCard.Rank.Zero]], or rank [[TarotCard.Rank.Fifteen]] and above.
 */
case class TarotCard(val suit: TarotCard.Suit, val rank: TarotCard.Rank, val isAlsoZero: Boolean = false) extends SuitedCard {
  require(isAlsoZero <= (suit == TarotCard.Suit.Joker), "Only jokers can optionally have zero as additional value.")
  require((suit != TarotCard.Suit.Joker) <= (rank.ordinal >= 1 && rank.ordinal <= 14), "Non-jokers only rank from Ace to King.")

  /**
   * The companion object [[TarotCard$]].
   */
  override val companion: TarotCard.type = TarotCard

  /**
   * The set of possible values that this [[TarotCard]] can be used as.
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
   * How many points this [[TarotCard]] is worth.
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

  /**
   * The everyday name of this [[TarotCard]].
   */
  override val toString: String = {
    if suit != TarotCard.Suit.Joker then 
      s"$rank of $suit"
    else
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

/**
 * Companion object of the class [[TarotCard]].
 */
object TarotCard extends SuitedCardCompanion[TarotCard] {
  /**
   * Creates a [[TarotCard]] with the specified [[Suit]] and [[Rank]].
   *
   * @param suit enum-value specifying the suit of the card.
   * @param rank enum-value specifying the rank of the card.
   * @return a new [[TarotCard]] with the specified [[Suit]] and [[Rank]].
   */
  override def apply(suit: Suit, rank: Rank): TarotCard = new TarotCard(suit, rank)

  /**
   * Creates a [[TarotCard]] with the specified [[Suit]] and [[Rank]].
   *
   * @param suit enum-value specifying the suit of the card.
   * @param rank enum-value specifying the rank of the card.
   * @param isAlsoZero If set to `true`, then the card has the additional value of 0 -- this is only allowed for jokers.
   * @return a new [[TarotCard]] with the specified [[Suit]] and [[Rank]].
   */
  def apply(suit: Suit, rank: Rank, isAlsoZero: Boolean = false): TarotCard = new TarotCard(suit, rank, isAlsoZero)

  /**
   * Creates a [[TarotCard]] from its name, as specified by [[stringToCard]].
   *
   * @param name the name of the [[TarotCard]] to create
   * @param isAlsoZero If set to `true`, then the card has the additional value of 0 -- this is only allowed for jokers.
   * @return a new [[TarotCard]] representing the card with the given name.
   */
  def apply(name: String, isAlsoZero: Boolean): TarotCard = stringToCard(name, isAlsoZero)

  /**
   * Enum type containing the possible suit values: [[Suit.Staves]], [[Suit.Coins]], [[Suit.Cups]], [[Suit.Swords]], and [[Suit.Joker]].
   */
  enum Suit extends java.lang.Enum[Suit] with SuitTrait {
    case Staves, Coins, Cups, Swords, Joker

    /**
     * `true` if this [[Suit]] equals [[Suit.Swords]].
     */
    override def isSpades: Boolean = this == Swords
  }

  /**
   * Enum type containing the possible rank values: [[Rank.Zero]], [[Rank.Ace]], [[Rank.Two]], ..., [[Rank.Ten]], [[Rank.Page]], [[Rank.Knight]], [[Rank.Queen]], [[Rank.King]], [[Rank.Fifteen]], ..., [[Rank.TwentyOne]].
   */
  enum Rank extends java.lang.Enum[Rank] with RankTrait {
    case Zero, Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Page, Knight, Queen, King, Fifteen, Sixteen, Seventeen, Eighteen, Nineteen, Twenty, TwentyOne
  }

  /**
   * Creates a [[TarotCard]] from its name. The name of a joker can be appended with `(0)` to designate that the joker can also be used as value 0.
   *
   * @param name the name of the [[TarotCard]] to create
   * @return a new [[TarotCard]] representing the card with the given name.
   * @note If `card` is an instance of [[TarotCard]], then `TarotCard.stringToCard(card.toString)` returns a copy of `card`.
   */
  override def stringToCard(name: String): TarotCard = {
    if name.endsWith("(0)") then
      stringToCard(name.stripSuffix("(0)"), true)
    else
      stringToCard(name, false)
  }

  /**
   * Creates a [[TarotCard]] from its name.
   *
   * @param name the name of the [[TarotCard]] to create
   * @param isAlsoZero If set to `true`, then the card has the additional value of 0 -- this is only allowed for jokers.
   * @return a new [[TarotCard]] representing the card with the given name.
   */
  def stringToCard(name: String, isAlsoZero: Boolean = false): TarotCard = {
    if name.contains(" of ") then
      val Array(rankString, suitString) = name.strip().split(" of ", 2)
      return TarotCard(Suit.valueOf(suitString.toLowerCase.capitalize), Rank.valueOf(rankString.toLowerCase.capitalize))
    name.strip().toLowerCase match {
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
      case _ => throw new IllegalArgumentException("Not a valid TarotCard name: " + name)
    }
  }

  /**
   * Creates a new default deck populated with one of each [[TarotCard]]. The deck is ordered by suit with the jokers at the end.
   *
   * @return a new default [[TarotCard]] deck.
   */
  override def newDeck: Seq[TarotCard] = {
    newDeck(Set())
  }

  /**
   * Creates a new default deck populated with one of each [[TarotCard]]. The deck is ordered by suit with the jokers at the end.
   *
   * @param extraZeroJokers a set of integers between 0 and 21 (inclusive). The jokers with these ranks can also be used as value 0 in addition to their normal values.
   * @return a new [[TarotCard]] deck, with the specified optional zero-valued jokers.
   */
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
