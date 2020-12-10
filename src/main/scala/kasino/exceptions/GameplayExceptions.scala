package kasino.exceptions

import kasino.game.Player

abstract class GameplayException(message: String) extends KasinoException(message) {
  
}

class TurnOrderException private (playerActing: Player, playerAtTurn: Player, message: String) extends GameplayException(message) {
  require(playerActing.id != playerAtTurn.id, "A TurnOrderException should not be called when the player acting is also at turn.")
  
  def this(playerActing: Player, playerAtTurn: Player, messageOverride: Option[String] = None) = 
    this(playerActing: Player, playerAtTurn: Player, messageOverride match {
      case Some(message) => message
      case None => s"${playerActing.name} attempted to act during ${playerAtTurn.name}'s turn."
    })
}
