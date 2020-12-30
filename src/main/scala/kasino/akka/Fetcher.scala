package kasino.akka

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

import java.util.concurrent.TimeoutException
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Fetcher {
  case class Fetch[Result](replyTo: ActorRef[Result])
}
import Fetcher.Fetch

@tailrec
def fetch[Query <: Message, Result] (target: ActorRef[Dispatch[Query]], query: ActorRef[Result] => Query)(context: ActorContext[?]) : Result = {
  import akka.actor.typed.scaladsl.AskPattern._
  import scala.concurrent.duration.DurationInt
  implicit def responseTimeout: akka.util.Timeout = 4.seconds
  implicit def system: Scheduler = context.system
  
  
  val fetcher: ActorRef[Result | Fetch[Result]] = context.spawn(Behaviors.setup(context => new Fetcher[Query, Result](target, query, context)), "Fetcher")
  var futureReply: Future[Result] = fetcher.ask[Result](replyTo => Fetch(replyTo))
  try {
    Await.result(futureReply, Duration.Inf)
  } catch {
    case TimeoutException => fetch(target, query)(context)
  }
}


class Fetcher[Query <: Message, Result] (target: ActorRef[Dispatch[Query]], query: ActorRef[Result] => Query, context: ActorContext[Result | Fetch[Result]]) extends AbstractBehavior[Result | Fetch[Result]](context) {
  private var replyTo: ActorRef[Result] = context.system.ignoreRef
  
  override def onMessage(msg: Result | Fetch[Result]): Behavior[Result | Fetch[Result]] = {
    msg match {
      case Fetch(replyTo: ActorRef[Result]) =>
        this.replyTo = replyTo
        dispatch(target, query(context.self))(context)
        this
      case result: Result => 
        replyTo ! result
        Behaviors.stopped
    }
  }
}
