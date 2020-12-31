package kasino.akka

import akka.Done
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.pattern.StatusReply

import java.util.concurrent.TimeoutException
import scala.util.{Failure, Success, Try}
import Dispatch.Letter

def dispatch[T <: Message]( recipient : ActorRef[Letter[T]], message: T)(implicit context: ActorContext[?]) : ActorRef[Try[Done]] = context.spawn(Behaviors.setup(context => new Dispatcher[T](recipient, message, context)),"Dispatcher-"+message.messageId)

class Dispatcher[T <: Message]( val recipient : ActorRef[Letter[T]], val message: T, context: ActorContext[Try[Done]]) extends AbstractBehavior[Try[Done]](context) {
  import scala.concurrent.duration.DurationInt
  implicit def responseTimeout: akka.util.Timeout = 3.seconds 
  
  def send(): Unit = {
    context.askWithStatus[Letter[T],Done](recipient, Letter(message, context.self, _))(identity)
  }
  send()

  override def onMessage(message: Try[Done]): Behavior[Try[Done]] = message match {
    case Success(_) => Behaviors.stopped
    case Failure(_: TimeoutException) => send(); this
    case Failure(e: Throwable) => throw e
  }
}
