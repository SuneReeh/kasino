package kasino.game

import kasino.cards.Card
import java.util.UUID
import scala.collection.SeqView

class Player (private val controller: Controller, 
              private val handView: SeqView[Card], 
              private val tableView: SeqView[CardStack],
              deckSize: =>Int) {
  def name: String = controller.name

  val id : UUID = controller.id
}
