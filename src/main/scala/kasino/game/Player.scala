package kasino.game

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import kasino.akka.{Dispatch, KasinoActor, fetch}
import kasino.cards.Card
import kasino.exceptions.KasinoException
import kasino.game.Game.{ActionProvider, CardPosition}
import kasino.game.Game.CardPosition._

import java.util.UUID
import scala.collection.SeqView
import scala.util.{Failure, Success, Try}


object Player {
  def apply(controller: ActorRef[Dispatch[Controller.Message]], handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: =>Int, currentPlayerId: =>UUID, currentPlayerName: =>String, actions: Game.ActionProvider): Behavior[Dispatch[Message]] = 
    Behaviors.setup(context => new Player(controller, handView, tableView, deckSize, currentPlayerId, currentPlayerName, actions, context))
  
  enum Action {
    case Play(posHand: Hand)
    case Add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None)
    case Mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None)
    case Combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None)
    case Take(posTable: Table, posHand: Hand)
    case FiveOfSpades(posHand : Hand)
    case Reset
    case End
  }

  enum Message extends kasino.akka.Message() {
    //case NameAndId(name: String, id: UUID)
    case UpdateGameState()
    case TakeTurn()
    case Act(action: Action)
    case ReportFailure()
    case GetId(replyTo: ActorRef[UUID])
    case GetName(replyTo: ActorRef[String])
  }
}


class Player (private val controller: ActorRef[Dispatch[Controller.Message]], 
              private val handView: SeqView[Card], 
              private val tableView: SeqView[CardStack],
              deckSize: =>Int,
              currentPlayerId: =>UUID,
              currentPlayerName: =>String,
              actions: Game.ActionProvider,
              implicit val context: ActorContext[Dispatch[Player.Message]]) extends KasinoActor[Player.Message] {
  lazy val name: String = fetch(controller, Controller.Message.GetName(_))

  val id : UUID = fetch(controller, Controller.Message.GetId(_))

  private def play(posHand: Hand): Try[Unit] = actions.play(posHand)()
  private def add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.add(pos1,pos2,res)()
  private def mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.mod(pos1,pos2,res)()
  private def combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.combine(pos1,pos2,res)()
  private def take(posTable: Table, posHand: Hand): Try[Unit] = actions.take(posTable, posHand)()
  private def fiveOfSpades(posHand : Hand): Try[Unit] = actions.fiveOfSpades(posHand)()
  private def reset(): Try[Unit] = actions.reset()
  private def end(): Try[Unit] = actions.end()

  override def actOnMessage(message: Player.Message): KasinoActor[Player.Message] = {
    import Player.Message._
    
    message match {
      case GetId(replyTo: ActorRef[UUID]) => replyTo ! id
      case GetName(replyTo: ActorRef[String]) => replyTo ! name
    }
    ???
    this
  }
  
  def updateGameState(): Unit = {
    controller.updateGameState(handView, tableView, deckSize, currentPlayerId, currentPlayerName)
  }
  
  def takeTurn(): Unit = {
    import Player.Action._
    
    while !controller.getReady() do ()
    while {
      val action = controller.getAction()
      val attempt: Try[Unit] = action match {
        case Play(posHand: Hand) => play(posHand)
        case Add(pos1: CardPosition, pos2: CardPosition, res) => add(pos1, pos2, res)
        case Mod(pos1: CardPosition, pos2: CardPosition, res) => mod(pos1, pos2, res)
        case Combine(pos1: CardPosition, pos2: CardPosition, res) => combine(pos1, pos2, res)
        case Take(posTable: Table, posHand: Hand) => take(posTable, posHand)
        case FiveOfSpades(posHand : Hand) => fiveOfSpades(posHand)
        case Reset => reset()
        case End => end()
      }
      attempt match {
        case Failure(exception: Exception) => controller.reportFailure(Failure(exception))
        case Failure(otherError) => otherError.printStackTrace(); throw otherError
        case _ => ()
      }
      !(action == End && attempt.isSuccess)
    } do ()
  }
}


