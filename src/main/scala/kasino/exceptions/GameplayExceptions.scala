package kasino.exceptions

import kasino.cards.Card
import kasino.game.{CardStack, Player}

abstract class GameplayException(message: String) extends KasinoException(message) 

class TurnOrderException private (playerActing: Player, playerAtTurn: Player, message: String) extends GameplayException(message) {
  require(playerActing.id != playerAtTurn.id, "A TurnOrderException should not be called when the player acting is also at turn.")
  
  def this(playerActing: Player, playerAtTurn: Player, messageOverride: Option[String] = None) = 
    this(playerActing: Player, playerAtTurn: Player, messageOverride match {
      case Some(message) => message
      case None => s"${playerActing.name} attempted to act during ${playerAtTurn.name}'s turn."
    })
}

class MultipleCardsPlayedException extends GameplayException("You cannot play more than one card per turn.")

class NoCardsPlayedException extends GameplayException("You must play a card each turn.")

class IllegalClaimException(stack: CardStack, card: Card) extends GameplayException("You cannot claim a CardStack with values " + stack.values + " using a card with values " + card.values + ".")

class AttemptToClearEmptyTableException extends GameplayException("You cannot clear an already empty table.")
