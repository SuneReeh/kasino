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
              actions: Game.ActionProvider) {
  def name: String = controller.name

  val id : UUID = controller.id

  private def Play(posHand: Hand): Try[Unit] = actions.Play(posHand)()
  private def Add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.Add(pos1,pos2,res)()
  private def Mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.Mod(pos1,pos2,res)()
  private def Combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Try[Unit] = actions.Combine(pos1,pos2,res)()
  private def Take(posTable: Table, posHand: Hand): Try[Unit] = actions.Take(posTable, posHand)()
  private def FiveOfSpades(posHand : Hand): Try[Unit] = actions.FiveOfSpades(posHand)()
  private def Reset: Try[Unit] = actions.Reset()
  private def End: Try[Unit] = actions.End()
}
