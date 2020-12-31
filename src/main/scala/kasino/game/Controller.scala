package kasino.game

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext}
import kasino.akka.{Dispatch, KasinoActor}
import kasino.cards.Card
import kasino.exceptions.KasinoException

import java.util.UUID
import scala.collection.SeqView
import scala.util.{Failure, Success, Try}

object Controller {
  enum Message extends kasino.akka.Message() {
    case AttachPlayer(player: ActorRef[Dispatch[Player.Message]])
    case UpdateGameState(handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: Int, currentPlayerId: UUID, currentPlayerName: String)
    case StartTurn()
    //case ContinueTurn(previousActionResult: Try[Unit])
    case ActionResult(action: Player.Action, result: Try[Unit])
    //case EndTurn()
    case ReportFailure(failed: Failure[Exception])
    case GetId(replyTo: ActorRef[UUID])
    case GetName(replyTo: ActorRef[String])
  }
}


abstract class Controller(override implicit val context: ActorContext[Dispatch[Controller.Message]]) extends KasinoActor[Controller.Message] {
  val id : UUID = UUID.randomUUID()

  private var _name : String = ""
  def name: String = _name
  protected def name_=(name: String): Unit = {_name = name}
  
  def updateGameState(handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: Int, currentPlayerId: UUID, currentPlayerName: String): Unit
  
  def getReady(): Boolean 
  
  def getAction(): Player.Action 
  
  def reportFailure(failed: Failure[Exception]): Unit
}

