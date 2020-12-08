package kasino.game

import java.util.UUID

trait Controller {
  val id : UUID = UUID.randomUUID()

}
