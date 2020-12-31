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

import scala.concurrent.Await
import scala.concurrent.duration.Duration

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
    val system: ActorSystem[MainActor.Message] = ActorSystem(Behaviors.setup(context => new MainActor()(context)), "MainActor")
    system ! MainActor.Message.Run
    Await.result(system.whenTerminated, Duration.Inf)
  }
}

