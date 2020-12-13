package kasino.game

import kasino.cards.Card
import kasino.game.Game.{Action, ActionProvider, CardPosition}
import kasino.game.Game.CardPosition._

import java.util.UUID
import scala.collection.SeqView
import scala.util.{Try, Success, Failure}

class Player (private val controller: Controller, 
              private val handView: SeqView[Card], 
              private val tableView: SeqView[CardStack],
              deckSize: =>Int,
              currentPlayerId: =>UUID,
              currentPlayerName: =>String,
              actions: Game.ActionProvider) {
  def name: String = controller.name

  val id : UUID = controller.id

  private def play(posHand: Hand): Try[Unit] = actions.play(posHand)()
  private def add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.add(pos1,pos2,res)()
  private def mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.mod(pos1,pos2,res)()
  private def combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.combine(pos1,pos2,res)()
  private def take(posTable: Table, posHand: Hand): Try[Unit] = actions.take(posTable, posHand)()
  private def fiveOfSpades(posHand : Hand): Try[Unit] = actions.fiveOfSpades(posHand)()
  private def reset(): Try[Unit] = actions.reset()
  private def end(): Try[Unit] = actions.end()
  
  def takeTurn(): Unit = ???
}

object Player {
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
}
