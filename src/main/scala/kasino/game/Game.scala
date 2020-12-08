package kasino.game

import kasino.cards.Card

import scala.collection.mutable
import scala.collection.mutable.{ArrayDeque, Map, Queue}

/**
 * A concrete game of "Nørdekasino".
 */
class Game (controllers: Iterable[Controller], newDeck: Iterable[Card]) {
  private val deck: ArrayDeque[Card] = Queue(scala.util.Random.shuffle(newDeck).toSeq: _*)
  private val table: ArrayDeque[CardStack] = ArrayDeque();
  private val hands: Map[Player, ArrayDeque[Card]] = Map.empty;
  private val players: scala.collection.immutable.Seq[Player] =
    controllers.map{c =>
      val hand: mutable.ArrayDeque[Card] = ArrayDeque();
      val player: Player = new Player(c, hand.view, table.view)
      hands.addOne(player, hand)
      player
    }.toSeq
}
