package kasino

import java.util.UUID

class Player {
  private var _name : String = ""
  def name: String = _name
  private def name_=(name: String): Unit = {_name = name}

  val Id : UUID = UUID.randomUUID()
}
