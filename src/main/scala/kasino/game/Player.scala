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
  def apply(controller: ActorRef[Dispatch[Controller.Message]], handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: =>Int, actions: Game.ActionProvider, game: ActorRef[Dispatch[Game.Message]]): Behavior[Dispatch[Message]] = 
    Behaviors.setup(context => new Player(controller, handView, tableView, deckSize, actions, game, context))
  
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
    case ActionResult(action: Game.Action, result: Try[Unit])
    case ReportFailure(failed: Failure[Exception])
    case GetId(replyTo: ActorRef[UUID])
    case GetName(replyTo: ActorRef[String])
  }
}


class Player (private val controller: ActorRef[Dispatch[Controller.Message]], 
              private val handView: SeqView[Card], 
              private val tableView: SeqView[CardStack],
              deckSize: =>Int,
              actions: Game.ActionProvider,
              private val game: ActorRef[Dispatch[Game.Message]],
              override implicit val context: ActorContext[Dispatch[Player.Message]]) extends KasinoActor[Player.Message] {
  
  sendMessage(controller, Controller.Message.AttachPlayer(context.self))
  
  lazy val name: String = fetch(controller, Controller.Message.GetName(_))
  val id : UUID = fetch(controller, Controller.Message.GetId(_))

  import scala.language.implicitConversions
  
  private implicit def playerToGameAction(playerAction: Player.Action): Game.Action = {
    import Player.Action._
    playerAction match {
      case Play(posHand: Hand) => actions.play(posHand)
      case Add(pos1: CardPosition, pos2: CardPosition, res: Option[Int]) => actions.add(pos1, pos2, res)
      case Mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int]) => actions.mod(pos1, pos2, res)
      case Combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int]) => actions.combine(pos1, pos2, res)
      case Take(posTable: Table, posHand: Hand) => actions.take(posTable, posHand)
      case FiveOfSpades(posHand : Hand) => actions.fiveOfSpades(posHand)
      case Reset => actions.reset
      case End => actions.end
    }
  }
  
  private implicit def gameToPlayerAction(gameAction: Game.Action): Player.Action = {
    gameAction match {
      case play: Game.Action.Play => Player.Action.Play(play.posHand)
      case add: Game.Action.Add => Player.Action.Add(add.pos1, add.pos2, add.res)
      case mod: Game.Action.Mod => Player.Action.Mod(mod.pos1, mod.pos2, mod.res)
      case combine: Game.Action.Combine => Player.Action.Combine(combine.pos1, combine.pos2, combine.res)
      case take: Game.Action.Take => Player.Action.Take(take.posTable, take.posHand)
      case fos: Game.Action.FiveOfSpades => Player.Action.FiveOfSpades(fos.posHand)
      case reset: Game.Action.Reset => Player.Action.Reset
      case end: Game.Action.End => Player.Action.End
    }
  }
  
  override def actOnMessage(message: Player.Message): KasinoActor[Player.Message] = {
    import Player.Message._
    
    message match {
      case GetId(replyTo: ActorRef[UUID]) => replyTo ! id
      case GetName(replyTo: ActorRef[String]) => replyTo ! name
      case UpdateGameState() => sendMessage(controller, Controller.Message.UpdateGameState(handView, tableView, deckSize, fetch(game, Game.Message.GetCurrentPlayerId(_)), fetch(game, Game.Message.GetCurrentPlayerName(_))))
      case TakeTurn() => sendMessage(controller, Controller.Message.StartTurn())
      case Act(action: Player.Action) => sendMessage(game, Game.Message.Act(action))
      case ActionResult(action: Game.Action, result: Try[Unit]) => sendMessage(controller, Controller.Message.ActionResult(action, result))
      case ReportFailure(failed: Failure[Exception]) => sendMessage(controller, Controller.Message.ReportFailure(failed))
    }
    this
  }
}


