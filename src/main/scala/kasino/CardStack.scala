package kasino

import java.util.UUID

import kasino.cards.Card

class CardStack private(val values: Set[Int],
val points: Int = 0,
val cards: Seq[Card],
val ownerId: Option[UUID] = None,
val ownerName: Option[String] = None,
val lockedValue: Boolean = false) {

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
}

object CardStack {
  def apply(card: Card, ownerName: Option[String] = None): CardStack = {
    new CardStack(card.values, card.points, Seq(card), ownerName = ownerName )
  }

  def apply (card: Card, owner: Player): CardStack = {
    new CardStack(card.values, card.points, Seq(card), ownerName = Some(owner.name), ownerId = Some(owner.Id) )
  }
}


