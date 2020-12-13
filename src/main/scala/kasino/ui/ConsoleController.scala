package kasino.ui

import kasino.cards.Card
import kasino.exceptions.KasinoException
import kasino.game.{CardStack, Controller, Player}

import java.util.UUID
import scala.collection.SeqView
import scala.util.{Try, Success, Failure}
import scala.io.StdIn

class ConsoleController extends Controller {
  getName()
  
  def getName(): Unit = {
    while ({
      val input = StdIn.readLine(s"Enter player (id: ${id}) name: ").strip()
      if input != "" then 
        name = input
        false
      else
        true
    }) do ()
  }
  
  private var latestGameState: String = ""
  
  override def updateGameState(handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: Int, currentPlayerId: UUID, currentPlayerName: String): Unit = {
    def pluralS(value: Int): String = if value != 1 then "s" else ""
    
    val state = StringBuilder(s"There are ${deckSize} card${pluralS(deckSize)} left in the deck.\n\n")
    state ++= "Stacks of kasino.cards on the table\n"
    for (stack, i) <- tableView.zipWithIndex do
      state ++= s"$i -- ${stack.toString.drop(12).dropRight(1)}\n"
    state ++= "\nCards in hand\n"
    for (card, i) <- handView.zipWithIndex do
      state ++= s"h$i -- $card  Value${pluralS(card.values.size)}: "
      state ++= card.values.mkString(", ")
      state ++= s"  Points: ${card.points}\n\n"
    if currentPlayerId != id then
      state ++= s"Is if ${currentPlayerName}'s turn.\n"
    latestGameState = state.result()
  }

  override def getReady(): Boolean = {
    val input = StdIn.readLine(s"${name}, it is your turn. Are you ready? ").toLowerCase().strip()
    Seq("", "y", "yes").contains(input)
  }

  override def getAction(): Player.Action = ???

  override def reportFailure(failed: Failure[KasinoException]): Unit = {
    println()
    println(failed.exception.getMessage)
    StdIn.readLine()
  }
}
