package kasino.exceptions

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import kasino.akka.{Dispatch, fetch}
import kasino.cards.Card
import kasino.game.{CardStack, Player}

abstract class GameplayException(message: String) extends KasinoException(message) 

class TurnOrderException private (playerActing: ActorRef[Dispatch[Player.Message]], playerAtTurn: ActorRef[Dispatch[Player.Message]], message: String)(implicit context: ActorContext[?]) extends GameplayException(message) {
  require(fetch(playerActing, Player.Message.GetId(_)) != fetch(playerAtTurn, Player.Message.GetId(_)), "A TurnOrderException should not be called when the player acting is also at turn.")
  
  def this(playerActing: ActorRef[Dispatch[Player.Message]], playerAtTurn: ActorRef[Dispatch[Player.Message]], messageOverride: Option[String] = None)(implicit context: ActorContext[?]) = 
    this(playerActing: ActorRef[Dispatch[Player.Message]], playerAtTurn: ActorRef[Dispatch[Player.Message]], messageOverride match {
      case Some(message) => message
      case None => s"${fetch(playerActing, Player.Message.GetName(_))} attempted to act during ${fetch(playerAtTurn, Player.Message.GetName(_))}'s turn."
    })
}

class MultipleCardsPlayedException extends GameplayException("You cannot play more than one card per turn.")

class NoCardsPlayedException extends GameplayException("You must play a card each turn.")

class IllegalClaimException(stack: CardStack, card: Card) extends GameplayException("You cannot claim a CardStack with values " + stack.values + " using a card with values " + card.values + ".")

class AttemptToClearEmptyTableException extends GameplayException("You cannot clear an already empty table.")

class MultipleStacksOwnedException(messageOverride: Option[String]=None) extends 
  GameplayException(if messageOverride != None then messageOverride.get else "Cannot leave behind multiple stacks owned by the player.")

class UnableToClaimException(stack: CardStack) extends GameplayException("Has no card in hand that can claim the owned stack: " + stack + ".")
