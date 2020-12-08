package kasino.exceptions

import kasino.game.CardStack
import kasino.exceptions.ArithmeticException.OpType
import kasino.exceptions.ArithmeticException.OpType._


/**
 * Abstract base class for all exceptions thrown by arithmetic operations between [[kasino.game.CardStack]]s.
 *
 * @param message the detail message.
 */
abstract class ArithmeticException(message: String) extends KasinoException(message) {
  /**
   * The type of [[kasino.game.CardStack]] operation giving rise to this exception.
   */
  val operation : OpType

  /**
   * The first [[kasino.game.CardStack]] involved in the operation throwing this exception.
   */
  val firstInput : CardStack

  /**
   * The second [[kasino.game.CardStack]] involved in the operation throwing this exception.
   */
  val secondInput : CardStack

  /**
   * The optional specified value of the [[kasino.game.CardStack]] supposed to result from the operation throwing this exception.
   */
  val resultValue : Option[Int]
}

/**
 * Companion object for [[ArithmeticException]].
 */
object ArithmeticException {

  /**
   * Enum for the possible operations between [[kasino.game.CardStack]]s: Sum (+), Mod (%), and Combine (&).
   */
  enum OpType {
    case Sum, Mod, Combine
  }
}



/**
 * Thrown when an operation between [[kasino.game.CardStack]]s has no legal value for the result -- either because there is no possible value at all, or because an impossible result value has been specified.
 */
class InvalidResultException private (val operation: OpType, val firstInput : CardStack, val secondInput: CardStack, val resultValue : Option[Int], message : String) extends ArithmeticException(message) {
  /**
   * Creates a new [[InvalidResultException]] with the specified attributes.
   *
   * @param operation the type of [[kasino.game.CardStack]] operation giving rise to this exception.
   * @param firstInput the first [[kasino.game.CardStack]] involved in the operation throwing this exception.
   * @param secondInput the second [[kasino.game.CardStack]] involved in the operation throwing this exception.
   * @param resultValue the optional result value of the operation throwing this exception.
   * @param messageOverride if present, overrides the detail message for this exception. Defaults to [[scala.None]].
   */
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

/**
 * Thrown when an operation between [[kasino.game.CardStack]]s has input stacks with multiple values, but no result value has been specified.
 */
class AmbiguousResultException private (val operation: OpType, val firstInput : CardStack, val secondInput: CardStack, message : String) extends ArithmeticException(message) {
  /**
   * Always [[scala.None]] for this exception.
   */
  val resultValue: None.type = None

  /**
   * Creates a new [[AmbiguousResultException]] with the specified attributes.
   *
   * @param operation the type of [[kasino.game.CardStack]] operation giving rise to this exception.
   * @param firstInput the first [[kasino.game.CardStack]] involved in the operation throwing this exception.
   * @param secondInput the second [[kasino.game.CardStack]] involved in the operation throwing this exception.
   * @param messageOverride if present, overrides the detail message for this exception. Defaults to [[scala.None]].
   */
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

/**
 * Thrown when an operation attempts to change the value of a [[kasino.game.CardStack]] whose value has been locked.
 */
class LockedValueException private (val operation: OpType, val firstInput : CardStack, val secondInput: CardStack, message : String) extends ArithmeticException(message) {
  /**
   * Irrelevant for this exception.
   */
  val resultValue: None.type = None

  /**
   * Creates a new [[LockedValueException]] with the specified attributes.
   *
   * @param operation the type of [[kasino.game.CardStack]] operation giving rise to this exception. Either [[OpType.Sum]] or [[OpType.Mod]].
   * @param firstInput the first [[kasino.game.CardStack]] involved in the operation throwing this exception.
   * @param secondInput the second [[kasino.game.CardStack]] involved in the operation throwing this exception.
   * @param messageOverride if present, overrides the detail message for this exception. Defaults to [[scala.None]].
   */
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
