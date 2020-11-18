package kasino.exceptions

import kasino.CardStack
import kasino.exceptions.ArithmeticException.OpType
import OpType._


abstract class ArithmeticException(message: String) extends KasinoException(message) {
  val operation : OpType
  val firstInput : CardStack
  val secondInput : CardStack
  val resultValue : Option[Int]
}

object ArithmeticException {
  enum OpType {
    case Sum, Mod, Combine
  }
}

class InvalidResultException private (val operation: OpType, val firstInput : CardStack, val secondInput: CardStack, val resultValue : Option[Int], message : String) extends ArithmeticException(message) {
  def this(operation: OpType, firstInput : CardStack, secondInput: CardStack, resultValue : Option[Int] = None, messageOverride: Option[String] = None) = {
    this(operation, firstInput, secondInput, resultValue, messageOverride match {
      case Some(message) => message
      case None => (operation, resultValue) match {
        case (Sum, Some(result)) => s"You cannot make ${result} as a sum of values ${firstInput.values} and ${secondInput.values}."
        case (Sum, None) => throw new IllegalArgumentException("You can always add non-ambiguous values.") //s"There is no legal sum of values ${firstInput.values} and ${secondInput.values}."
        case (Mod, Some(result)) => s"You cannot make ${result} as a non-trivial remainder of values ${firstInput.values} by ${secondInput.values}."
        case (Mod, None) => s"There is no non-trivial remainder of values ${firstInput.values} by ${secondInput.values}."
        case (Combine, Some(result)) => s"Cannot make ${"}more $result${"} from stacks with values ${firstInput.values} and ${secondInput.values}."
        case (Combine, None) => s"Cannot combine stacks with values ${firstInput.values} and ${secondInput.values}."
      }
    })
  }
}

class AmbiguousResultException private (val operation: OpType, val firstInput : CardStack, val secondInput: CardStack, message : String) extends ArithmeticException(message) {
  val resultValue: None.type = None

  def this(operation: OpType, firstInput : CardStack, secondInput: CardStack, messageOverride: Option[String] = None) = {
    this(operation, firstInput, secondInput, messageOverride match {
      case Some(message) => message
      case None => operation match {
        case Sum => s"Cannot add stacks with multiple values ${firstInput.values} and ${secondInput.values} without specifying the result."
        case Mod => s"Cannot take remainder of stacks with multiple values ${firstInput.values} and ${secondInput.values} without specifying the result."
        case Combine => s"Cannot combine stacks with multiple values ${firstInput.values} and ${secondInput.values} without specifying a common value."
      }
    })
  }
}

class LockedValueException private (val operation: OpType, val firstInput : CardStack, val secondInput: CardStack, message : String) extends ArithmeticException(message) {
  val resultValue: None.type = None

  def this(operation: OpType, firstInput : CardStack, secondInput: CardStack, messageOverride: Option[String] = None) = {
    this(operation, firstInput, secondInput, messageOverride match {
      case Some(message) => message
      case None =>
        (firstInput.lockedValue, secondInput.lockedValue) match {
        case (true, true) => s"You cannot change the value of either ${firstInput} or ${secondInput}."
        case (true, false) => s"You cannot change the value of ${firstInput}."
        case (false, true) => s"You cannot change the value of ${secondInput}."
        case (false, false) => throw new IllegalArgumentException("LockedValueException must come from at least one stack with a locked value.")
      }
    })
  }
}
