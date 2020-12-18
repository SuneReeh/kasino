package kasino.game

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext}
import kasino.akka.{MessageHandling, MessageReceipt}
import kasino.cards.Card
import kasino.exceptions.KasinoException

import java.util.UUID
import scala.collection.SeqView
import scala.util.{Failure, Success, Try}

object Controller {
  enum Message extends kasino.akka.Message() {
    case UpdateGameState(override val messageId: UUID)
    case StartTurn(override val messageId: UUID)
    case ContinueTurn(override val messageId: UUID)
    case EndTurn(override val messageId: UUID)
    case ReportFailure(override val messageId: UUID)

    case Recieved(receipt: Player.Receipt)
  }

  case class Receipt(override val messageId: UUID) extends MessageReceipt(messageId)
}


abstract class Controller(context: ActorContext[Controller.Message]) extends AbstractBehavior[Controller.Message](context) with MessageHandling[Controller.Message](context) {
  val id : UUID = UUID.randomUUID()

  private var _name : String = ""
  def name: String = _name
  protected def name_=(name: String): Unit = {_name = name}
  
  def updateGameState(handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: Int, currentPlayerId: UUID, currentPlayerName: String): Unit
  
  def getReady(): Boolean 
  
  def getAction(): Player.Action 
  
  def reportFailure(failed: Failure[Exception]): Unit
}

