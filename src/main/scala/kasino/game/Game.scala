package kasino.game

import kasino.cards.Card
import kasino.game.Game.{ActionProvider, CardPosition, Action}
import kasino.game.Game.CardPosition._

import reeh.math.BigRational

import java.util.UUID
import scala.collection.mutable.{ArrayDeque, Map, Queue}
import scala.util.{Failure, Success, Try}

/**
 * A concrete game of "Nørdekasino".
 * 
 * @param controllers controllers for the [[Player]]s in this game.
 * @param newDeck a deck of [[kasino.cards.Card]]s with which to play a game.
 */
class Game (controllers: Iterable[Controller], newDeck: Iterable[Card]) {
  private val deck: ArrayDeque[Card] = Queue(scala.util.Random.shuffle(newDeck).toSeq: _*)
  private val table: ArrayDeque[CardStack] = ArrayDeque()
  private val hands: Map[UUID, ArrayDeque[Card]] = Map.empty
  private val players: scala.collection.immutable.Seq[Player] =
    scala.util.Random.shuffle(controllers).map{c =>
      val hand: ArrayDeque[Card] = ArrayDeque()
      val player: Player = new Player(c, hand.view, table.view, deckSize, generatePlayerActions(c.id))
      hands.addOne(player.id, hand)
      player
    }.toSeq
  private val numPlayers = players.size
  private var currentPlayerPos: Int = 0
  private var lastToClaimPos: Option[Int] = None
  private val cardsToClaim: ArrayDeque[Card] = ArrayDeque()
  private var usedCard: Option[Card] = None

  //Backup game state from the beginning of each turn 
  private var tableBackup: ArrayDeque[CardStack] = ArrayDeque()
  private var lastToClaimPosBackup: Option[Int] = None
  
  //End of game
  private var _gameFinished: Boolean = false
  def gameFinished: Boolean = _gameFinished
  private val scores: Map[UUID,BigRational] = Map.empty
  private var _resultReport: Option[String] = None
  def resultReport: Option[String] = _resultReport
  
  /** The number of [[kasino.cards.Card]]s remaining in the deck. */
  def deckSize : Int = deck.size
  
  /** The [[java.util.UUID]] of the current [[Player]]. */
  def currentPlayerId: UUID = players(currentPlayerPos).id
  
  /** The name of the current [[Player]]. */
  def currentPlayerName: String = players(currentPlayerPos).name
  
  private def generatePlayerActions(playerId: UUID): ActionProvider = new ActionProvider {
    override def Play(posHand: Hand): Game.Action = ???

    override def Add(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Game.Action = ???

    override def Mod(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Game.Action = ???

    override def Combine(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Game.Action = ???

    override def Take(posTable: Table, posHand: Hand): Game.Action = ???

    override def FiveOfSpades(posHand: Hand): Game.Action = ???

    override def Reset: Game.Action = ???

    override def End: Game.Action = ???
  }
}

object Game {
  enum CardPosition {
    case Table(i: Int)
    case Hand(i: Int) 
  } 
  
  sealed trait Action {
    def apply(): Try[Unit]
  }
  
  
  sealed trait ActionProvider {
    def Play(posHand : Hand): Action
    def Add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def Mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def Combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def Take(posTable: Table, posHand: Hand): Action
    def FiveOfSpades(posHand : Hand): Action
    def Reset: Action
    def End: Action
  }
}

