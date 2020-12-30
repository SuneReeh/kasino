package kasino.akka

import akka.Done
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext}
import akka.pattern.StatusReply

import java.util.UUID
import scala.util.Try

import Dispatch._

abstract class KasinoActor[Recieves <: Message](implicit val context: ActorContext[Dispatch[Recieves]]) extends AbstractBehavior[Dispatch[Recieves]](context) {
  //val dispatchesRecieved: scala.collection.mutable.Set[ActorRef[?]] = scala.collection.mutable.Set()
  val messagesRecieved: scala.collection.mutable.Set[UUID] = scala.collection.mutable.Set()
  //val sentAwaitingReceipts: scala.collection.mutable.Map[UUID, Letter[? <: Message]] = scala.collection.mutable.Map()

  //case class MessageTimer(messageId: UUID)

  def actOnMessage(message: Recieves): KasinoActor[Recieves]

  def onMessage(letter: Dispatch[Recieves]): KasinoActor[Recieves] = letter match {
    case Letter(message: Recieves, dispatcher: ActorRef[Try[Done]], receiptTo: ActorRef[StatusReply[Done]]) =>
      receiptTo ! StatusReply.Ack
      if messagesRecieved.contains(message.messageId) then
        this
      else
        messagesRecieved += message.messageId
        context.watchWith(dispatcher, DispatchComplete(message.messageId))
        actOnMessage(message)
    case DispatchComplete(messageId: UUID) => 
      messagesRecieved -= messageId
      this
  }

  def sendMessage[Target <: Message](recipient: ActorRef[Letter[Target]], message: Target): Unit = {
    dispatch(recipient, message)
  }
} 
