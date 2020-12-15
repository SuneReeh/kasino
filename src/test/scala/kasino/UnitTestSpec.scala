package kasino

import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import org.scalatest.prop._

abstract class UnitTestSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks
