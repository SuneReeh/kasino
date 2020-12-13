package kasino.game

import kasino.cards.Card
import kasino.exceptions.KasinoException

import java.util.UUID
import scala.collection.SeqView
import scala.util.{Try, Success, Failure}

trait Controller {
  val id : UUID = UUID.randomUUID()

  private var _name : String = ""
  def name: String = _name
  protected def name_=(name: String): Unit = {_name = name}
  
  def updateGameState(handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: Int, currentPlayerId: UUID, currentPlayerName: String): Unit
  
  def getReady(): Boolean 
  
  def getAction(): Player.Action 
  
  def reportFailure(failed: Failure[KasinoException]): Unit
}
