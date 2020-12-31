package kasino

import kasino.akka.Dispatch
import kasino.cards.{Card, ModernCard, TarotCard}
import kasino.game.{Controller, Game}
import kasino.ui.ConsoleController

import _root_.akka.actor.typed.{ActorRef, ActorSystem, Behavior, Signal, Terminated}
import _root_.akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

import scala.collection.mutable.ArrayDeque
import scala.io.StdIn
import scala.util.{Random, Success, Try}

object MainActor {
  enum Message {
    case GameFinished
  }
}

class MainActor(implicit context: ActorContext[MainActor.Message]) extends AbstractBehavior[MainActor.Message](context) {

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
  val game: ActorRef[Dispatch[Game.Message]] = context.spawn(Game(context.self, controllers, deck),"Game")
  akka.dispatch[Game.Message](game, Game.Message.Run())

  override def onMessage(msg: MainActor.Message): Behavior[MainActor.Message] = {
    import MainActor.Message._
    import akka.fetch

    msg match {
      case GameFinished =>
        import _root_.akka.actor.typed.scaladsl.AskPattern._
        import kasino.akka.fetch

        assert(fetch[Game.Message.GetGameFinished,Boolean](game, Game.Message.GetGameFinished(_)))
        Main.clearConsole()
        println(fetch[Game.Message.GetResultReport,Option[String]](game, Game.Message.GetResultReport(_)).getOrElse("Results missing!"))
        context.stop(game)
        return Behaviors.stopped
    }
    this
  }
}
