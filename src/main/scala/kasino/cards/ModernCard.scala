package kasino.cards

/**
 * The usual modern day playing cards with four suits (clubs, diamonds, hearts, and spades) of ranks from ace to king, and jokers.
 *
 * @constructor Create a new [[ModernCard]] with specified suit and rank.
 * @param suit the suit of this [[ModernCard]].
 * @param rank the rank of this [[ModernCard]].
 * @note Jokers have the suit [[ModernCard.Suit.Joker]] and must have the unique rank [[ModernCard.Rank.Zero]]. Additionally, jokers are the only cards allow to have rank zero.
 */
case class ModernCard (val suit: ModernCard.Suit, val rank: ModernCard.Rank) extends SuitedCard {
  require((suit == ModernCard.Suit.Joker) == (rank.ordinal == 0), "A card has rank zero if and only if it is a joker.")
  
  /**
   * The companion object [[ModernCard$]].
   */
  override val companion: ModernCard.type = ModernCard

  /**
   * The set of possible values that this [[ModernCard]] can be used as.
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
   * How many points this [[ModernCard]] is worth.
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

  /**
   * The everyday name of this [[ModernCard]].
   */
  override val toString: String = if suit == ModernCard.Suit.Joker then "Joker" else s"$rank of $suit"
}

/**
 * Companion object of the class [[ModernCard]].
 */
object ModernCard extends SuitedCardCompanion[ModernCard] {
  /**
   * Creates a [[ModernCard]] with the specified [[Suit]] and [[Rank]].
   *
   * @param suit enum-value specifying the suit of the card.
   * @param rank enum-value specifying the rank of the card.
   * @return a new [[ModernCard]] with the specified [[Suit]] and [[Rank]]
   */
  override def apply(suit: Suit, rank: Rank): ModernCard = new ModernCard(suit,rank)

  /**
   * Enum type containing the possible suit values: [[Suit.Clubs]], [[Suit.Diamonds]], [[Suit.Hearts]], [[Suit.Spades]], and [[Suit.Joker]].
   */
  enum Suit extends java.lang.Enum[Suit] with SuitTrait 
  {
    case Clubs, Diamonds, Hearts, Spades, Joker

    /**
     * [[true]] if this [[Suit]] equals [[Spades]].
     */
    override def isSpades: Boolean = this == Spades
  }

  /**
   * Enum type containing the possible rank values: [[Rank.Zero]], [[Rank.Ace]], [[Rank.Two]], [[Rank.Three]], ..., [[Rank.Ten]], [[Rank.Jack]], [[Rank.Queen]], [[Rank.King]].
   */
  enum Rank extends java.lang.Enum[Rank] with RankTrait
  {
    case Zero, Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King
  }

  /**
   * Creates a [[ModernCard]] from its name.
   *
   * @param name the name of the [[ModernCard]] to create
   * @return a new [[ModernCard]] representing the card with the given name.
   * @note If `card` is an instance of [[ModernCard]], then {{{ModernCard.stringToCard(card.toString)}}} returns a copy of `card`.
   */
  override def stringToCard(string: String): ModernCard = {
    if string.strip().toLowerCase == "joker" then
      return ModernCard(Suit.Joker, Rank.Zero)
    val Array(rankString,suitString) = string.split(" of ", 2)
    ModernCard(Suit.valueOf(suitString.toLowerCase.capitalize), Rank.valueOf(rankString.toLowerCase.capitalize))
  }

  /**
   * Creates a new default deck populated with [[ModernCard]]s. The deck contains three jokers, and is ordered by suit with jokers at the end.
   *
   * @return a new default [[ModernCard]] deck.
   */
  override def newDeck: Seq[ModernCard] = {
    var deck: Seq[ModernCard] = for {
      suit <- Suit.values.toSeq if suit != Suit.Joker
      rank <- Rank.values.toSeq if rank != Rank.Zero
    } yield ModernCard(suit,rank)
    deck = deck :+ ModernCard("Joker")
    deck = deck :+ ModernCard("Joker")
    deck = deck :+ ModernCard("Joker")
    deck
  }

  /**
   * Creates a new default deck populated with [[ModernCard]]s, but without jokers. The deck is ordered by suit with jokers at the end.
   *
   * @return a new default [[ModernCard]] deck without jokers.
   */
  def newDeckWithoutJokers: Seq[ModernCard] = {
    var deck: Seq[ModernCard] = for {
      suit <- Suit.values.toSeq if suit != Suit.Joker
      rank <- Rank.values.toSeq if rank != Rank.Zero
    } yield ModernCard(suit, rank)
    deck
  }
}
