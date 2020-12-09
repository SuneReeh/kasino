package kasino.game

import kasino.cards.Card

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.{ArrayDeque, Map, Queue}

/**
 * A concrete game of "Nørdekasino".
 * 
 * @param controllers controllers for the [[Player]]s in this game.
 * @param newDeck a deck of [[kasino.cards.Card]]s with which to play a game.
 */
class Game (controllers: Iterable[Controller], newDeck: Iterable[Card]) {
  private val deck: ArrayDeque[Card] = Queue(scala.util.Random.shuffle(newDeck).toSeq: _*)
  private val table: ArrayDeque[CardStack] = ArrayDeque()
  private val hands: Map[Player, ArrayDeque[Card]] = Map.empty
  private val players: scala.collection.immutable.Seq[Player] =
    scala.util.Random.shuffle(controllers).map{c =>
      val hand: mutable.ArrayDeque[Card] = ArrayDeque()
      val player: Player = new Player(c, hand.view, table.view, deckSize)
      hands.addOne(player, hand)
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
  private var gameFinished: Boolean = false;
  //private val scores: Map[Player,???] = Map.empty
  
  /** The number of [[kasino.cards.Card]]s remaining in the deck. */
  def deckSize : Int = deck.size
  
  /** The [[java.util.UUID]] of the current [[Player]]. */
  def currentPlayerId: UUID = players(currentPlayerPos).id
  
  /** The name of the current [[Player]]. */
  def currentPlayerName: String = players(currentPlayerPos).name
}
