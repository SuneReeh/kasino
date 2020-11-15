import kasino.cards._

object Main {
  def main(args: Array[String]): Unit = {
    println(ModernCard.Suit.getClass)
    println(ModernCard.Suit.Spades.isSpades)
    println(ModernCard("joker"))
    println(ModernCard("Six of Hearts").points)
    println(ModernCard("SIX of hearts"))
    println(ModernCard("ten of spades").values)
    println(ModernCard("ten of hearts").points)
  }
}
