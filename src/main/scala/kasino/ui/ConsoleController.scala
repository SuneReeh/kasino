package kasino.ui

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.ActorContext
import kasino.cards.Card
import kasino.exceptions.KasinoException
import kasino.game.Game.CardPosition
import kasino.game.Game.CardPosition._
import kasino.game.{CardStack, Controller, Player}

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.SeqView
import scala.util.{Failure, Success, Try}
import scala.io.StdIn

class ConsoleController(context: ActorContext[Controller.Message]) extends Controller(context) {
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

  override def onMessage(msg: Controller.Message): Behavior[Controller.Message] = ???
  
  override def updateGameState(handView: SeqView[Card], tableView: SeqView[CardStack], deckSize: Int, currentPlayerId: UUID, currentPlayerName: String): Unit = {
    def pluralS(value: Int): String = if value != 1 then "s" else ""
    
    val state = StringBuilder(s"There ${if deckSize == 1 then "is" else "are"} ${deckSize} card${pluralS(deckSize)} left in the deck.\n\n")
    state ++= "Stacks of cards on the table\n"
    for (stack, i) <- tableView.zipWithIndex do
      state ++= s"$i -- ${stack.toString.drop(12).dropRight(1)}\n"
    state ++= "\nCards in hand\n"
    for (card, i) <- handView.zipWithIndex do
      state ++= s"h$i -- $card  Value${pluralS(card.values.size)}: "
      state ++= card.values.mkString(", ")
      state ++= s"  Points: ${card.points}\n"
    if currentPlayerId != id then
      state ++= s"Is if ${currentPlayerName}'s turn.\n"
    latestGameState = state.result()
  }

  override def getReady(): Boolean = {
    clearConsole()
    val input = StdIn.readLine(s"${name}, it is your turn. Are you ready? ").toLowerCase().strip()
    Seq("", "y", "yes").contains(input)
  }

  // @tailrec
  override final def getAction(): Player.Action = {
    import Player.Action._
    
    clearConsole()
    println(latestGameState)
    val input = StdIn.readLine("add/a, mod/m, combine/c, take/t, play/p, five_of_spades/fos, reset/r, end/e > ").strip().toLowerCase()
    val tokens = input.split(" ")
    
    if tokens.isEmpty then {
      reportFailure(Failure(new KasinoException("Invalid action.")))  
      return getAction()
    }

    if Seq("r", "reset").contains(tokens(0)) && tokens.size == 1 then return Reset
    
    if Seq("e", "end").contains(tokens(0)) && tokens.size == 1 then return End
    
    if Seq("a", "add").contains(tokens(0)) then {
      if tokens.size == 1 then 
        StdIn.readLine("Write 'add [index] [index] [optional: result]' to add two stacks/cards together -- at most one card from the hand.\nPress Enter to Continue\n")
        return getAction()
      if 3 <= tokens.size && tokens.size <= 4 then {
        val pos1: Try[CardPosition] = {
          if tokens(1).head == 'h' then
            Try(Hand(tokens(1).drop(1).toInt))
          else
            Try(Table(tokens(1).toInt))
        }
        val pos2: Try[CardPosition] = {
          if tokens(2).head == 'h' then
            Try(Hand(tokens(2).drop(1).toInt))
          else
            Try(Table(tokens(2).toInt))
        }
        if pos1.isFailure || pos2.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        if tokens.size == 3 then
          return Add(pos1.get, pos2.get)
        val res: Try[Int] = Try(tokens(3).toInt)
        if res.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        return Add(pos1.get, pos2.get, Some(res.get))
      }
    }

    if Seq("m", "mod").contains(tokens(0)) then {
      if tokens.size == 1 then
        StdIn.readLine("Write 'mod [index] [index] [optional: result]' to take the remainder by division of two stacks/cards -- at most one card from the hand.\nPress Enter to Continue\n")
        return getAction()
      if 3 <= tokens.size && tokens.size <= 4 then {
        val pos1: Try[CardPosition] = {
          if tokens(1).head == 'h' then
            Try(Hand(tokens(1).drop(1).toInt))
          else
            Try(Table(tokens(1).toInt))
        }
        val pos2: Try[CardPosition] = {
          if tokens(2).head == 'h' then
            Try(Hand(tokens(2).drop(1).toInt))
          else
            Try(Table(tokens(2).toInt))
        }
        if pos1.isFailure || pos2.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        if tokens.size == 3 then
          return Mod(pos1.get, pos2.get)
        val res: Try[Int] = Try(tokens(3).toInt)
        if res.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        return Mod(pos1.get, pos2.get, Some(res.get))
      }
    }

    if Seq("c", "combine").contains(tokens(0)) then {
      if tokens.size == 1 then
        StdIn.readLine("Write 'combine [index] [index] [optional: value]' to combine two stacks/cards sharing a common value -- at most one card from the hand.\nPress Enter to Continue\n")
        return getAction()
      if 3 <= tokens.size && tokens.size <= 4 then {
        val pos1: Try[CardPosition] = {
          if tokens(1).head == 'h' then
            Try(Hand(tokens(1).drop(1).toInt))
          else
            Try(Table(tokens(1).toInt))
        }
        val pos2: Try[CardPosition] = {
          if tokens(2).head == 'h' then
            Try(Hand(tokens(2).drop(1).toInt))
          else
            Try(Table(tokens(2).toInt))
        }
        if pos1.isFailure || pos2.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        if tokens.size == 3 then
          return Combine(pos1.get, pos2.get)
        val res: Try[Int] = Try(tokens(3).toInt)
        if res.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        return Combine(pos1.get, pos2.get, Some(res.get))
      }
    }

    if Seq("t", "take").contains(tokens(0)) then {
      if tokens.size == 1 then
        StdIn.readLine("Write 'take [index_on_table] [index_in_hand]' to take a stack on the table with a matching card from the hand.\nPress Enter to Continue\n")
        return getAction()
      if tokens.size == 3 then {
        val posTable: Try[Table] = {
          Try(Table(tokens(1).toInt))
        }
        val posHand: Try[Hand] = {
          if tokens(2).head == 'h' then
            Try(Hand(tokens(2).drop(1).toInt))
          else
            Failure(new KasinoException("Invalid action."))
        }
        if posTable.isFailure || posHand.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        return Take(posTable.get, posHand.get)
      }
    }

    if Seq("p", "play").contains(tokens(0)) then {
      if tokens.size == 1 then
        StdIn.readLine("Write 'play [index_in_hand]' to play a single card from the hand.\nPress Enter to Continue\n")
        return getAction()
      if tokens.size == 2 then {
        val posHand: Try[Hand] = {
          if tokens(1).head == 'h' then
            Try(Hand(tokens(1).drop(1).toInt))
          else
            Failure(new KasinoException("Invalid action."))
        }
        if posHand.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        return Play(posHand.get)
      }
    }

    if Seq("fos", "five_of_spades").contains(tokens(0)) then {
      if tokens.size == 1 then
        StdIn.readLine("Write 'five_of_spades [index_in_hand]' to clear a non-empty table with the five of spades. The [index_in_hand] must point to the five of spades.\nPress Enter to Continue\n")
        return getAction()
      if tokens.size == 2 then {
        val posHand: Try[Hand] = {
          if tokens(1).head == 'h' then
            Try(Hand(tokens(1).drop(1).toInt))
          else
            Failure(new KasinoException("Invalid action."))
        }
        if posHand.isFailure then {
          reportFailure(Failure(new KasinoException("Invalid action.")))
          return getAction()
        }
        return FiveOfSpades(posHand.get)
      }
    }

    reportFailure(Failure(new KasinoException("Invalid action.")))
    return getAction()
  }

  override def reportFailure(failed: Failure[Exception]): Unit = {
    println()
    print(failed.exception.getMessage + "\nPress Enter to Continue\n")
    StdIn.readLine()
  }
  
  private def clearConsole(): Unit = {
    //print("\u001b[2J")
    kasino.Main.clearConsole()
  }
}
