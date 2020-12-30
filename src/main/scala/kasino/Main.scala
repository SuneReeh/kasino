package kasino

import kasino.cards.{Card, ModernCard, TarotCard}
import kasino.game.{Controller, Game}
import kasino.ui.ConsoleController

import scala.collection.mutable.ArrayDeque
import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}
import _root_.akka.actor.typed.{ActorRef, ActorSystem, Behavior, Signal, Terminated}
import _root_.akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import kasino.akka.Dispatch

object Main {
  private val isWindows: Boolean = System.getProperty("os.name").toLowerCase.startsWith("win")
  
  def clearConsole(): Unit = {
    if isWindows then {
      //scala.sys.process.stringSeqToProcess(Seq("cmd", "/c", "cls")).!<
      new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor()
    };
    else
      print("\u001b[2J")
      print("\u001b[H")
  }
  
  def main(args: Array[String]): Unit = {
    if isWindows then
      Console.withIn(new java.io.InputStreamReader(System.in, java.nio.charset.Charset.forName("windows-1252"))) {
        Console.withOut(new java.io.PrintStream(System.out, false, java.nio.charset.Charset.forName("windows-1252"))) {
          runGame()
        }
      }
    else
      runGame()
  }

  def runGame(): Unit = {
    ActorSystem(Behaviors.setup(context => new MainActor(context)), "MainActor")
  }
}

object MainActor {
  enum Message {
    case GameFinished
  }
}

class MainActor(context: ActorContext[MainActor.Message]) extends AbstractBehavior[MainActor.Message](context) {
  
  Main.clearConsole()
  println("Welcome to 'Noerdekasino'")
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
  val controllers: ArrayDeque[ActorRef[Dispatch[Controller.Message]]] = ArrayDeque()
  for i <- 1 to numPlayers do {
    controllers.append(context.spawn(ConsoleController(),"Controller-"+i))
  }
  val game: ActorRef[Dispatch[Game.Message]] = context.spawn(Game(controllers, deck),"Game")
  akka.dispatch[Game.Message](game, Game.Message.Run())
  
  override def onMessage(msg: MainActor.Message): Behavior[MainActor.Message] = {
    import MainActor.Message._
    import akka.fetch
    
    msg match {
      case GameFinished =>
        import _root_.akka.actor.typed.scaladsl.AskPattern._
        assert(fetch[Game.Message.GetGameFinished,Boolean](game, Game.Message.GetGameFinished(_))(context))
        clearConsole()
        println(game.resultReport.getOrElse("Results missing!"))
        return Behaviors.stopped
    }
    this
  }

  
  
  
}
