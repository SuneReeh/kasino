package kasino.game

import kasino.cards.Card
import kasino.exceptions.{MultipleCardsPlayedException, TurnOrderException}
import kasino.game.Game.{Action, ActionProvider, CardPosition}
import kasino.game.Game.CardPosition._
import reeh.math.BigRational

import java.util.UUID
import scala.collection.mutable.{ArrayDeque, Map, Queue}
import scala.util.{Failure, Success, Try}

/**
 * A concrete game of "Nørdekasino".
 * 
 * @param controllers controllers for the [[Player]]s in this game.
 * @param newDeck a deck of [[kasino.cards.Card]]s with which to play a game. Needs at least `4* controllers.size +2` cards.
 */
class Game (controllers: Iterable[Controller], newDeck: Iterable[Card]) {
  require(newDeck.size >= 4 * controllers.size + 2, "The provided deck is too small for a game with " + controllers.size + " players.")
  
  private class Deck (deck: Iterable[Card]) extends Queue[Card](deck.size) {
    this.appendAll(deck)
    
    def draw(): Card = removeHead()
  }
  private val deck: Deck = new Deck(scala.util.Random.shuffle(newDeck))
  private val table: ArrayDeque[CardStack] = ArrayDeque()
  private val hands: Map[UUID, ArrayDeque[Card]] = Map.empty
  private val playersById: Map[UUID, Player] = Map.empty
  private val players: scala.collection.immutable.Seq[Player] =
    scala.util.Random.shuffle(controllers).map{c =>
      val hand: ArrayDeque[Card] = ArrayDeque()
      val player: Player = new Player(c, hand.view, table.view, deckSize, generatePlayerActions(c.id))
      hands.addOne(player.id, hand)
      playersById.addOne(player.id, player)
      player
    }.toSeq
  private val numPlayers = players.size
  
  //Start of game
  private var _gameStarted: Boolean = false
  def gameStarted: Boolean = _gameStarted
  
  //State for current turn
  private var currentPlayerPos: Int = 0
  private var lastToClaim: Option[UUID] = None
  private val cardsToClaim: ArrayDeque[Card] = ArrayDeque()
  private var usedCard: Option[Card] = None

  //Backup game state from the beginning of each turn 
  private var tableBackup: ArrayDeque[CardStack] = ArrayDeque()
  private var lastToClaimBackup: Option[UUID] = None
  
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
  
  def start(): Try[Unit] = {
    if _gameStarted then
      return Failure(new RuntimeException("Game already running."))
    for player <- players do 
      for i <- 1 to 4 do 
        hands(player.id).append(deck.draw())
    for i <- 1 to 2 do
      table.append(CardStack(deck.draw()))
    currentPlayerPos = 0
    usedCard = None
    lastToClaim = None
    lastToClaimBackup = None
    cardsToClaim.clear()
    tableBackup = table.clone()
    _gameStarted = true
    players(currentPlayerPos).takeTurn()
    return Success(())
  }
  
  private def resetTurn(): Unit = {
    table.clear()
    table.appendAll(tableBackup)
    for card <- usedCard do
      hands(currentPlayerId).append(card)
    lastToClaim = lastToClaimBackup
    cardsToClaim.clear()
  }
  
  private def generatePlayerActions(playerId: UUID): ActionProvider = new ActionProvider {
    private def checkHasTurn(): Try[Unit] =
      if playerId != currentPlayerId then
        return Failure(new TurnOrderException(playersById(playerId), players(currentPlayerPos)))
      Success(())
    
    private def checkNoUsedCard(): Try[Unit] =
      if usedCard != None then
        return Failure(new MultipleCardsPlayedException)
      Success(())
    
    private def checkFor42(posTable: Table): Boolean = ???
    
    override def play(posHand: Hand): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        
        val card = Try(hands(playerId).remove(posHand.i))
        if card.isFailure then return Failure(card.failed.get)
        usedCard = Some(card.get)
        table.append(CardStack(card.get,playersById(playerId)))
        Success(())
      }
    }

    override def add(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        
        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] =
          val playerHand = hands(playerId)
          val result = Try( (table(tablePos) + CardStack(playerHand(handPos))) (res,Some(playersById(playerId))) )
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos,result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          checkFor42(Table(tablePos))
          Success(())

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] =
          val result = Try( (table(tablePosMin) + table(tablePosMax)) (res,Some(playersById(playerId))) )
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePosMin,result.get)
          table.remove(tablePosMax)
          checkFor42(Table(tablePosMin))
          Success(())
        
        pos1 match {
          case Hand(handPos) => pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) => 
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot add a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def mod(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        ???
      }
    }

    override def combine(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        ???
      }
    }

    override def take(posTable: Table, posHand: Hand): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        ???
      }
    }

    override def fiveOfSpades(posHand: Hand): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        ???
      }
    }

    override def reset: Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        Try(resetTurn())
      }
    }

    override def end: Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        ???
      }
    }
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
    def play(posHand : Hand): Action
    def add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def take(posTable: Table, posHand: Hand): Action
    def fiveOfSpades(posHand : Hand): Action
    def reset: Action
    def end: Action
  }
}

