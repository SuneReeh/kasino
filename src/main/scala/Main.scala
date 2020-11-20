package kasino

import kasino.CardStack
import kasino.cards._

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    println(Random.shuffle(ModernCard.newDeck))
    println(Random.shuffle(TarotCard.newDeck(Set(Random.nextInt(22), Random.nextInt(22), Random.nextInt(22), Random.nextInt(22), Random.nextInt(22)))))
    println(TarotCard("temperance") == TarotCard(TarotCard.Suit.Joker, TarotCard.Rank.King))
    println(TarotCard("temperance") == TarotCard(TarotCard.Suit.Joker, TarotCard.Rank.King, true))
    val withZero = TarotCard("temperance", true)
    val withoutZero = TarotCard("temperance")
    println(withoutZero.values)
    println(withZero.values)
    println(TarotCard.unapply(TarotCard("temperance")))
    TarotCard("temperance") match {
      case TarotCard(s ,r, b) => println(s"$s, $r, $b")
    }
    println(CardStack(TarotCard("temperance")).cards)
    println(CardStack(TarotCard("temperance")) + CardStack(TarotCard("justice")) + CardStack(TarotCard("ten of cups")))
    println(CardStack(TarotCard("temperance")) % CardStack(TarotCard("justice")))
    println(CardStack(TarotCard("temperance")) % CardStack(TarotCard("ten of cups")))
    println((CardStack(TarotCard("ten of swords")) & CardStack(TarotCard("ten of cups")))(result = Some(10)))

    println(TarotCard(withoutZero.toString))
    println(TarotCard(withZero.toString))
  }
}
