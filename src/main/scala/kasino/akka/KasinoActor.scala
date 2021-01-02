package kasino.akka

import akka.Done
import akka.actor.typed.{ActorRef, Behavior, Signal, Terminated}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext}
import akka.pattern.StatusReply
import kasino.akka.Dispatch._

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.ArrayDeque
import scala.util.Try


abstract class KasinoActor[Recieves <: Message](override implicit val context: ActorContext[Dispatch[Recieves]]) extends AbstractBehavior[Dispatch[Recieves]](context) {
  //val dispatchesRecieved: scala.collection.mutable.Set[ActorRef[?]] = scala.collection.mutable.Set()
  val messagesRecieved: scala.collection.mutable.Set[UUID] = scala.collection.mutable.Set()
  //val sentAwaitingReceipts: scala.collection.mutable.Map[UUID, Letter[? <: Message]] = scala.collection.mutable.Map()
  private object Outbox {
    private val internalMap : scala.collection.mutable.Map[ActorRef[Letter[Nothing]], ArrayDeque[? <: Message]] = scala.collection.mutable.Map.empty
    
    def addContact[Target <: Message](target: ActorRef[Letter[Target]]): Unit = {
      if !internalMap.contains(target) then
        internalMap(target) = ArrayDeque[Target]()
        context.watch(target)
    }
    
    def removeContact[Target <: Message](target: ActorRef[Letter[Target]]): Unit = {
      internalMap.remove(target)
    }
    
    def addMsgToQueue[Target <: Message](target: ActorRef[Letter[Target]], message: Target): Unit = {
      require(internalMap.contains(target))
      
      val queue: ArrayDeque[Target] = 
        internalMap(target) match {
          case q: ArrayDeque[Target] => q
        }
      if queue.isEmpty then { 
        queue.append(message)
        sendNextMsg(target)
      } else if !queue.exists(oldMsg => (oldMsg.messageId == message.messageId)) then
        queue.append(message)
    }
    
    def sendNextMsg[Target <: Message](target: ActorRef[Letter[Target]]): Unit = {
      val message: Target = 
        internalMap(target).head match {
          case m : Target => m
        } 
      val dispatcher = dispatch(target, message)
      context.watchWith(dispatcher, OutgoingComplete(message.messageId, target))
    }
    
    def sendCompleted[Target <: Message](target: ActorRef[Letter[Target]], messageId: UUID): Unit = {
      val queue: ArrayDeque[Target] =
        internalMap(target) match {
          case q: ArrayDeque[Target] => q
        }
      
      queue.removeFirst(msg => (msg.messageId == messageId))
      if !queue.isEmpty then
        sendNextMsg(target)
    }
  }

  //case class MessageTimer(messageId: UUID)

  def actOnMessage(message: Recieves): KasinoActor[Recieves]

  def onMessage(letter: Dispatch[Recieves]): KasinoActor[Recieves] = letter match {
    case Letter(message: Recieves, dispatcher: ActorRef[Try[Done]], receiptTo: ActorRef[StatusReply[Done]]) =>
      receiptTo ! StatusReply.Ack
      if messagesRecieved.contains(message.messageId) then
        this
      else
        messagesRecieved += message.messageId
        context.watchWith(dispatcher, IncomingComplete(message.messageId))
        actOnMessage(message)
    case IncomingComplete(messageId: UUID) => 
      messagesRecieved -= messageId
      this
    case OutgoingComplete(messageId: UUID, target: ActorRef[Letter[?]]) =>
      Outbox.sendCompleted(target, messageId)
      this
  }

  override def onSignal: PartialFunction[Signal, Behavior[Dispatch[Recieves]]] = 
    case Terminated(actor: ActorRef[Letter[Nothing]]) =>
      Outbox.removeContact(actor)
      this
  

  def sendMessage[Target <: Message](recipient: ActorRef[Letter[Target]], message: Target): Unit = {
    Outbox.addContact(recipient)
    Outbox.addMsgToQueue(recipient, message)
  }
} 
