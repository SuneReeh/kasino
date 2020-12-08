package kasino.game

import java.util.UUID

class Player {
  private var _name : String = ""
  def name: String = _name
  private def name_=(name: String): Unit = {_name = name}
  private val controller : Controller = ???

  val id : UUID = UUID.randomUUID()
}
