package kasino.game

import kasino.cards.Card

import scala.collection.mutable.{ArrayDeque, Queue}

/**
 * A concrete game of "Nørdekasino".
 */
class Game (controllers: Iterable[Controller], newDeck: Iterable[Card]) {
  //private val deck: ArrayDeque[Card] = Queue(scala.util.Random.shuffle(newDeck))
  private val table: ArrayDeque[CardStack] = ArrayDeque();
  //public 
}
