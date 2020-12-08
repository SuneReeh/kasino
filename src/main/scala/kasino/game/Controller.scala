package kasino.game

import java.util.UUID

trait Controller {
  val id : UUID = UUID.randomUUID()

  private var _name : String = ""
  def name: String = _name
  private def name_=(name: String): Unit = {_name = name}
}
