package kasino

import java.util.UUID

import kasino.cards.Card
import kasino.exceptions.{AmbiguousResultException, InvalidResultException, LockedValueException}
import kasino.exceptions.ArithmeticException.OpType

class CardStack private(val values: Set[Int],
val points: Int = 0,
val cards: Seq[Card],
ownerIdInput: Option[UUID] = None,
ownerNameInput: Option[String] = None,
val lockedValue: Boolean = false) {
  
  private var _ownerId: Option[UUID] = ownerIdInput
  private var _ownerName: Option[String] = ownerNameInput
  
  def ownerId: Option[UUID] = _ownerId
  def ownerName: Option[String] = _ownerName
  def clearOwner(): Unit = {
    _ownerId = None
    _ownerName = None
  }

  override def toString: String = {
    val string: StringBuilder = StringBuilder("CardStack: (Value" + (if values.size != 1 then "s" else "") + ": ")
    val val_string: StringBuilder = StringBuilder()
    if lockedValue then
      val_string.append( "more ")
    for (value <- values) do
      val_string.append( value.toString + ", ")
    string.append( val_string.dropRight(2).result() + "  Points: " + points + "  Cards: ")
    val card_string: StringBuilder = StringBuilder()
    for (card <- cards) do
      card_string.append( card.toString + ", ")
    string.append( card_string.dropRight(2))
    for (name <- ownerName) do
      string.append( "  Owner: " + name)
    string.append(")").result()
  }

  import scala.annotation.infix
  @infix
  def +(other: CardStack): CardStack = {
    (this + other)()
  }

  @infix
  def +(other: CardStack)(result: Option[Int] = None, owner: Option[Player] = None): CardStack = {
    if this.lockedValue || other.lockedValue then
      throw new LockedValueException(OpType.Sum, this, other)
    if result.isEmpty then
      if values.size != 1 || other.values.size != 1 then
        throw new AmbiguousResultException(OpType.Sum,this, other)
      else
        var resultValue = 0
        for v1 <- values do
          for v2 <- other.values do
            resultValue = v1 + v2
        return new CardStack(Set(resultValue), points + other.points, cards ++ other.cards, owner.map(_.Id), owner.map(_.name))
    else
      var resultIsValid = false
      for v1 <- values do
        for v2 <- other.values do
          if result.get == v1 + v2 then
            resultIsValid = true
      if resultIsValid then
        return new CardStack(Set(result.get), points + other.points, cards ++ other.cards, owner.map(_.Id), owner.map(_.name))
      else
        throw new InvalidResultException(OpType.Sum, this, other, result)
  }
}

object CardStack {
  def apply(card: Card, ownerName: Option[String] = None): CardStack = {
    new CardStack(card.values, card.points, Seq(card), ownerNameInput = ownerName )
  }

  def apply (card: Card, owner: Player): CardStack = {
    new CardStack(card.values, card.points, Seq(card), ownerNameInput = Some(owner.name), ownerIdInput = Some(owner.Id) )
  }
}


