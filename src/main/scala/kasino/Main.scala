package kasino

import kasino.cards.{Card, ModernCard, TarotCard}
import kasino.game.{Controller, Game}
import kasino.ui.ConsoleController

import scala.collection.mutable.ArrayDeque
import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

object Main {

  private def clearConsole(): Unit = print("\u001b[2J")
  
  def main(args: Array[String]): Unit = {
    clearConsole()
    println("Welcome to 'Nørdekasino'")
    val numPlayers: Int = {
      var input: Try[Int] = Success(0)
      while {
        input = Try(StdIn.readLine("How many players? ").toInt)
        input.isFailure
      } do ()
      input.get
    }
    val deck: Seq[Card] = {
      var tempDeck: Seq[Card] = Seq()
      while {
        val input = StdIn.readLine("Which deck of cards (Modern/Tarot)? ").strip().toLowerCase
        if input == "modern" then {
          tempDeck = ModernCard.newDeck
          false
        } else if input == "tarot" then {
          tempDeck = TarotCard.newDeck(Set(Random.nextInt(22),Random.nextInt(22),Random.nextInt(22),Random.nextInt(22),Random.nextInt(22)))
          false
        } else true
      } do ()
      tempDeck
    } 
    val controllers: ArrayDeque[Controller] = ArrayDeque()
    for i <- 1 to numPlayers do {
      controllers.append(new ConsoleController)
    }
    val game: Game = new Game(controllers, deck)
    game.run()
    
    assert(game.gameFinished)
    clearConsole()
    println(game.resultReport.getOrElse("Results missing!"))
  }
}
