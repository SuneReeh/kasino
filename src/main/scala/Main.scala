import kasino.cards._

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    println(ModernCard.Suit.getClass)
    println(ModernCard.Suit.Spades.isSpades)
    println(ModernCard("joker"))
    println(ModernCard("Six of Hearts").points)
    println(ModernCard("SIX of hearts"))
    println(ModernCard("ten of spades").values)
    println(ModernCard("ten of hearts").points)
    println(Random.shuffle(ModernCard.newDeck))
    println(Random.shuffle(TarotCard.newDeck(Set(Random.nextInt(22), Random.nextInt(22), Random.nextInt(22), Random.nextInt(22), Random.nextInt(22)))))
    println(TarotCard("temperance") == TarotCard(TarotCard.Suit.Joker, TarotCard.Rank.King))
    println(TarotCard("temperance") == TarotCard(TarotCard.Suit.Joker, TarotCard.Rank.King, true))
    println(TarotCard("temperance").values)
    println(TarotCard("temperance", true).values)
  }
}
