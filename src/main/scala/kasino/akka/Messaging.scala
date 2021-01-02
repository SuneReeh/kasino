package kasino.akka

import akka.Done
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext}
import akka.pattern.StatusReply

import java.util.UUID
import scala.util.Try


//trait Protocol extends scala.reflect.Enum with Message {
//  type Receipt <: Protocol.Receipt
//}

/*
object Protocol {
  

  case class Receipt(val messageId: UUID)
}
*/

trait Message(val messageId: UUID = UUID.randomUUID())

enum Dispatch[+T <: Message] {
  case Letter ( val message: T, val dispatcher: ActorRef[Try[Done]], val receiptTo: ActorRef[StatusReply[Done]]) 
  
  case IncomingComplete(val messageId: UUID)
  case OutgoingComplete[+Origin <: Message, -Target <: Message](val messageId: UUID, val target: ActorRef[Letter[Target]]) extends Dispatch[Origin]
}
/*
case class Letter[T <: Message] ( val message: T)(val receiptTo: ActorRef[StatusReply[Done]]) {
  //type messageType = T
  
  val messageId: UUID = message.messageId
}
*/
