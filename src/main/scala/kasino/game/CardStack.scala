package kasino.game

import akka.actor.typed.ActorRef
import kasino.akka.Dispatch
import kasino.cards.Card
import kasino.exceptions.ArithmeticException.OpType
import kasino.exceptions.{AmbiguousResultException, InvalidResultException, LockedValueException}

import java.util.UUID

/**
 * A stack of cards on the table in a game of "Nørdekasino".
 */
class CardStack private(val values: Set[Int],
                        val points: Int = 0,
                        val cards: Seq[Card],
                        ownerIdInput: Option[UUID] = None,
                        ownerNameInput: Option[String] = None,
                        val lockedValue: Boolean = false) {

  private var _ownerId: Option[UUID] = ownerIdInput
  private var _ownerName: Option[String] = ownerNameInput


  /**
   * The [[java.util.UUID]] of the [[Player]] who made this [[CardStack]], if the stack is locked to a player.
   */
  def ownerId: Option[UUID] = _ownerId

  /**
   * The name of the [[Player]] who made this [[CardStack]], if the stack is locked to a player.
   */
  def ownerName: Option[String] = _ownerName

  /**
   * Clear the [[ownerId]] and [[ownerName]] if this [[CardStack]] becomes unassociated from the player.
   */
  def clearOwner(): Unit = {
    _ownerId = None
    _ownerName = None
  }

  /**
   * The gameplay information of this [[CardStack]] -- including, in order: the value(s) the stack can be used as, the number of points the stack is worth, which card(s) the stack contains, the name of the [[Player]] the stack is associated with (if any).
   */
  override def toString: String = {
    val string: StringBuilder = StringBuilder("CardStack: (Value" + (if values.size != 1 then "s" else "") + ": ")
    val val_string: StringBuilder = StringBuilder()
    if lockedValue then
      val_string.append("more ")
    for (value <- values) do
      val_string.append(value.toString + ", ")
    string.append(val_string.dropRight(2).result() + "  Points: " + points + "  Cards: ")
    val card_string: StringBuilder = StringBuilder()
    for (card <- cards) do
      card_string.append(card.toString + ", ")
    string.append(card_string.dropRight(2))
    for (name <- ownerName) do
      string.append("  Owner: " + name)
    string.append(")").result()
  }

  import scala.annotation.targetName

  /**
   * Returns a new [[CardStack]] resulting from addition of this [[CardStack]] with `other`.
   *
   * @param other another [[CardStack]] to add with this.
   * @return a new [[CardStack]] resulting from addition of this with `other`.
   * @throws kasino.exceptions.LockedValueException if either [[CardStack]] has `lockedValue = true`.
   * @throws kasino.exceptions.AmbiguousResultException if either [[CardStack]] has more than one possible value.
   */
  @targetName("add")
  infix def +(other: CardStack): CardStack = {
    (this + other) ()
  }

  /**
   * Returns a new [[CardStack]] resulting from addition of this [[CardStack]] with `other`.
   *
   * @param other another [[CardStack]] to add with this.
   * @param result optional specification of the value for the resulting [[CardStack]].
   * @param owner optional [[Player]] performing the addition.
   * @return a new [[CardStack]] resulting from addition of this with `other`, optionally with the specified value and owner.
   * @throws kasino.exceptions.LockedValueException if either [[CardStack]] has `lockedValue = true`.
   * @throws kasino.exceptions.AmbiguousResultException if either [[CardStack]] has more than one possible value and `result = None`.
   * @throws kasino.exceptions.InvalidResultException if `result` is not a possible value for the sum of the two [[CardStack]]s.
   */
  @targetName("add")
  infix def +(other: CardStack)(result: Option[Int] = None, owner: Option[ActorRef[Dispatch[Player.Message]]] = None): CardStack = {
    if this.lockedValue || other.lockedValue then
      throw new LockedValueException(OpType.Sum, this, other)
    if result.isEmpty then
      if values.size != 1 || other.values.size != 1 then
        throw new AmbiguousResultException(OpType.Sum, this, other)
      else
        var resultValue = 0
        for v1 <- values do
          for v2 <- other.values do
            resultValue = v1 + v2
        return new CardStack(Set(resultValue), points + other.points, cards ++ other.cards, owner.map(_.id), owner.map(_.name))
    else
      var resultIsValid = false
      for v1 <- values do
        for v2 <- other.values do
          if result.get == v1 + v2 then
            resultIsValid = true
      if resultIsValid then
        return new CardStack(Set(result.get), points + other.points, cards ++ other.cards, owner.map(_.id), owner.map(_.name))
      else
        throw new InvalidResultException(OpType.Sum, this, other, result)
  }

  /**
   * Returns a new [[CardStack]] resulting from taking the remainder of this [[CardStack]] upon dividing with `other`.
   *
   * @param other another [[CardStack]] with which to take remainder by.
   * @return a new [[CardStack]] as remainder of [[this]] modulo `other`.
   * @throws kasino.exceptions.LockedValueException if either [[CardStack]] has `lockedValue = true`.
   * @throws kasino.exceptions.AmbiguousResultException if either [[CardStack]] has more than one possible value.
   * @throws kasino.exceptions.InvalidResultException if `other` has bigger value than `this` so that the remainder is just trivially the same as `this.value`, or if `other` has value 0 so that the remainder is undefined.
   */
  @targetName("remainder")
  infix def %(other: CardStack): CardStack = {
    (this % other) ()
  }

  /**
   * Returns a new [[CardStack]] resulting from taking the remainder of this [[CardStack]] upon dividing with `other`.
   *
   * @param other another [[CardStack]] with which to take remainder by.
   * @param result optional specification of the value for the resulting [[CardStack]].
   * @param owner optional [[Player]] performing the remainder operation.
   * @return a new [[CardStack]] as remainder of `this` modulo `other`.
   * @throws kasino.exceptions.LockedValueException if either [[CardStack]] has `lockedValue = true`.
   * @throws kasino.exceptions.AmbiguousResultException if either [[CardStack]] has more than one possible value and `result = None`.
   * @throws kasino.exceptions.InvalidResultException if `result` is not a possible value for the sum of the two [[CardStack]]s. Alternatively, if `other` has bigger value than `this` so that the remainder is just trivially the same as `this.value`, or if `other` has value 0 so that the remainder is undefined.
   */
  @targetName("remainder")
  infix def %(other: CardStack)(result: Option[Int] = None, owner: Option[ActorRef[Dispatch[Player.Message]]] = None): CardStack = {
    if this.lockedValue || other.lockedValue then
      throw new LockedValueException(OpType.Mod, this, other)
    if result.isEmpty then
      if values.size != 1 || other.values.size != 1 then
        throw new AmbiguousResultException(OpType.Mod, this, other)
      else
        var resultValue = 0
        for v1 <- values do
          for v2 <- other.values do
            if v2 == 0 then
              throw new InvalidResultException(OpType.Mod, this, other, None, Some("Cannot take remainder by division with zero."))
            if v2 > v1 then
              throw new InvalidResultException(OpType.Mod, this, other, None)
            resultValue = v1 % v2
        return new CardStack(Set(resultValue), points + other.points, cards ++ other.cards, owner.map(_.id), owner.map(_.name))
    else
      var resultIsValid = false
      for v1 <- values do
        for v2 <- other.values do
          if v2 != 0 && v2 <= v1 && result.get == v1 % v2 then
            resultIsValid = true
      if resultIsValid then
        return new CardStack(Set(result.get), points + other.points, cards ++ other.cards, owner.map(_.id), owner.map(_.name))
      else
        throw new InvalidResultException(OpType.Mod, this, other, result)
  }

  /**
   * Returns a new [[CardStack]] resulting from combining this [[CardStack]] with `other`, both having the same value.
   *
   * @param other another [[CardStack]] with the same value as this.
   * @return a new [[CardStack]] resulting from combining this with `other`, the resulting stack has the same value as both input [[CardStack]]s.
   * @throws kasino.exceptions.AmbiguousResultException if either [[CardStack]] has more than one possible value.
   * @throws kasino.exceptions.InvalidResultException if 'other' and 'this' have different values.
   */
  @targetName("combine")
  infix def &(other: CardStack): CardStack = {
    (this & other) ()
  }

  /**
   * Returns a new [[CardStack]] resulting from combining this [[CardStack]] with `other`, both having a common value (specified optionally by the `result` parameter).
   *
   * @param other another [[CardStack]] with the same value as this.
   * @param result optional specification of the value for the resulting [[CardStack]]. This must be a value for both `this` and `other`.
   * @param owner optional [[Player]] combining the [[CardStack]]s.
   * @return a new [[CardStack]] resulting from combining this with `other`, the resulting stack has the same value as both input [[CardStack]]s -- or value `result` is specified.
   * @throws kasino.exceptions.AmbiguousResultException if either [[CardStack]] has more than one possible value and `result = None`.
   * @throws kasino.exceptions.InvalidResultException if 'other' and 'this' have different values, or if `result` is not a common value for both.
   */
  @targetName("combine")
  infix def &(other: CardStack)(result: Option[Int] = None, owner: Option[ActorRef[Dispatch[Player.Message]]] = None): CardStack = {
    if result.isEmpty then
      if values.size != 1 || other.values.size != 1 then
        throw new AmbiguousResultException(OpType.Combine, this, other)
      else
        for v1 <- values do
          for v2 <- other.values do
            if v1 == v2 then
              return new CardStack(Set(v1), points + other.points, cards ++ other.cards, owner.map(_.id), owner.map(_.name), true)
            else
              throw new InvalidResultException(OpType.Combine, this, other, None)
        return (null: CardStack) //Unreachable
    else
      var resultIsValid = false
      for v1 <- values do
        for v2 <- other.values do
          if result.get == v1 && v1 == v2 then
            resultIsValid = true
      if resultIsValid then
        return new CardStack(Set(result.get), points + other.points, cards ++ other.cards, owner.map(_.id), owner.map(_.name), true)
      else
        throw new InvalidResultException(OpType.Sum, this, other, result)
  }
}

/**
 * Companion object for [[CardStack]].
 */
object CardStack {
  /**
   * Creates a new [[CardStack]] from a single [[kasino.cards.Card]].
   *
   * @param card the [[kasino.cards.Card]] that this [[CardStack]] is built from.
   * @param ownerName optional name of the [[Player]] constructing this [[CardStack]].
   * @return a new [[CardStack]] with the same values and points as the provided card, containing the card, and (optionally) with the specified player name as ownerName.
   */
  def apply(card: Card, ownerName: Option[String] = None): CardStack = {
    new CardStack(card.values, card.points, Seq(card), ownerNameInput = ownerName)
  }

  /**
   * Creates a new [[CardStack]] from a single [[kasino.cards.Card]].
   *
   * @param card the [[kasino.cards.Card]] that this [[CardStack]] is built from.
   * @param owner optional [[Player]] constructing this [[CardStack]].
   * @return a new [[CardStack]] with the same values and points as the provided card, containing the card, and (optionally) with the specified player providing ownerId and ownerName.
   */
  def apply(card: Card, owner: ActorRef[Dispatch[Player.Message]]): CardStack = {
    new CardStack(card.values, card.points, Seq(card), ownerNameInput = Some(owner.name), ownerIdInput = Some(owner.id))
  }
}


