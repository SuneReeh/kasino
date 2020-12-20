package kasino.akka

import akka.Done
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.pattern.StatusReply

import java.util.concurrent.TimeoutException
import scala.util.{Failure, Success, Try}
import Dispatch.Letter

def dispatch[T <: Message]( recipient : ActorRef[Letter[T]], message: T) : Behavior[Try[Done]] = Behaviors.setup(context => new Dispatcher[T](recipient, message, context))

class Dispatcher[T <: Message]( val recipient : ActorRef[Letter[T]], val message: T, context: ActorContext[Try[Done]]) extends AbstractBehavior[Try[Done]](context) {
  import scala.concurrent.duration.DurationInt
  implicit def responseTimeout: akka.util.Timeout = 3.seconds 
  
  def send(): Unit = {
    context.askWithStatus[Letter[T],Done](recipient, Letter(message, context.self, _))(identity)
  }
  send()

  override def onMessage(message: Try[Done]): Behavior[Try[Done]] = message match {
    case Success => Behaviors.stopped
    case Failure(TimeoutException) => send(); this
    case Failure(e: Throwable) => throw e
  }
}
