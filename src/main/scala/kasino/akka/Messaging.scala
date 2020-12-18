package kasino.akka

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext

import java.util.UUID

trait Message(val messageId: UUID = UUID.randomUUID())

trait MessageReceipt(val messageId: UUID)

case class Letter[T <: Message] (val messageId: UUID, val recipient : ActorRef[T], val message: T) {
  type messageType = T
}

trait MessageHandling[T] (context: ActorContext[T])  {
  val messagesRecieved: scala.collection.mutable.Set[UUID] = scala.collection.mutable.Set()
  val awaitingReceipts: scala.collection.mutable.Map[UUID, Letter[? <: Message]] = scala.collection.mutable.Map()
} 
