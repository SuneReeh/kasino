package kasino.game

import kasino.cards.Card
import kasino.exceptions.{AttemptToClearEmptyTableException, IllegalClaimException, MultipleCardsPlayedException, MultipleStacksOwnedException, NoCardsPlayedException, TurnOrderException, UnableToClaimException}
import kasino.game.Game.{Action, ActionProvider, CardPosition}
import kasino.game.Game.CardPosition._
import reeh.math.BigRational

import java.util.UUID
import scala.collection.mutable.{ArrayDeque, Map, Queue}
import scala.util.{Failure, Success, Try}

/**
 * A concrete game of "Nørdekasino".
 * 
 * @param controllers controllers for the [[Player]]s in this game.
 * @param newDeck a deck of [[kasino.cards.Card]]s with which to play a game. Needs at least `4* controllers.size +2` cards.
 */
class Game (controllers: Iterable[Controller], newDeck: Iterable[Card]) {
  require(newDeck.size >= 4 * controllers.size + 2, "The provided deck is too small for a game with " + controllers.size + " players.")
  
  private class Deck (deck: Iterable[Card]) extends Queue[Card](deck.size) {
    this.appendAll(deck)
    
    def draw(): Card = removeHead()
  }
  private val deck: Deck = new Deck(scala.util.Random.shuffle(newDeck))
  private val table: ArrayDeque[CardStack] = ArrayDeque()
  private val hands: Map[UUID, ArrayDeque[Card]] = Map.empty
  private val playersById: Map[UUID, Player] = Map.empty
  private val players: scala.collection.immutable.Seq[Player] =
    scala.util.Random.shuffle(controllers).map{c =>
      val hand: ArrayDeque[Card] = ArrayDeque()
      val player: Player = new Player(c, hand.view, table.view, deckSize, currentPlayerId, currentPlayerName, generatePlayerActions(c.id))
      hands.addOne(player.id, hand)
      playersById.addOne(player.id, player)
      claimedCards.addOne(player.id, ArrayDeque())
      clears.addOne(player.id, 0)
      player
    }.toSeq
  private val numPlayers = players.size
  
  //Start of game
  private var _gameStarted: Boolean = false
  def gameStarted: Boolean = _gameStarted
  private def gameStarted_=(gameStarted: Boolean): Unit = _gameStarted = gameStarted
  
  //State for current turn
  private var currentPlayerPos: Int = 0
  private var lastToClaim: Option[UUID] = None
  private val cardsToClaim: ArrayDeque[Card] = ArrayDeque()
  private var usedCard: Option[Card] = None

  //Backup game state from the beginning of each turn 
  private var tableBackup: ArrayDeque[CardStack] = ArrayDeque()
  private var lastToClaimBackup: Option[UUID] = None
  
  //State for claimed cards
  private val claimedCards: Map[UUID,ArrayDeque[Card]] = Map.empty
  private var clears: Map[UUID, Int] = Map.empty
  
  //End of game
  private var _gameFinished: Boolean = false
  def gameFinished: Boolean = _gameFinished
  private def gameFinished_=(gameFinished: Boolean): Unit = _gameFinished = gameFinished
  private val scores: Map[UUID,BigRational] = Map.empty
  private var _resultReport: Option[String] = None
  def resultReport: Option[String] = _resultReport
  private def resultReport_=(resultReport: Option[String]): Unit = _resultReport = resultReport
  
  /** The number of [[kasino.cards.Card]]s remaining in the deck. */
  def deckSize : Int = deck.size
  
  /** The [[java.util.UUID]] of the current [[Player]]. */
  def currentPlayerId: UUID = players(currentPlayerPos).id
  
  /** The name of the current [[Player]]. */
  def currentPlayerName: String = players(currentPlayerPos).name
  
  def setup(): Try[Unit] = {
    if gameStarted then
      return Failure(new RuntimeException("Game already running."))
    for player <- players do 
      for i <- 1 to 4 do 
        hands(player.id).append(deck.draw())
    for i <- 1 to 2 do
      table.append(CardStack(deck.draw()))
    currentPlayerPos = 0
    usedCard = None
    lastToClaim = None
    lastToClaimBackup = None
    cardsToClaim.clear()
    tableBackup = table.clone()
    gameStarted = true
    players(currentPlayerPos).takeTurn()
    return Success(())
  }

  def run(): Unit = {
    setup()
    while !gameFinished do
      players(currentPlayerPos).takeTurn()
  }
  
  private def resetTurn(): Unit = {
    table.clear()
    table.appendAll(tableBackup)
    for card <- usedCard do
      hands(currentPlayerId).append(card)
    lastToClaim = lastToClaimBackup
    cardsToClaim.clear()
  }
  
  private def endTurn(): Try[Unit] = {
    if usedCard == None then
      return Failure(new NoCardsPlayedException)
    val playerId = currentPlayerId
    var numOwnedStacks = 0
    var ownedPos: Option[Int] = None
    var ownedStack: Option[CardStack] = None
    for (stack, i) <- table.view.zipWithIndex do
      if stack.ownerId == playerId then
        numOwnedStacks += 1
        ownedStack = Some(stack)
        ownedPos = Some(i)
    if numOwnedStacks > 1 then
      return Failure(new MultipleStacksOwnedException)
    if !cardsToClaim.isEmpty && numOwnedStacks > 0 && ownedStack.get != tableBackup(ownedPos.get) then
      return Failure(new MultipleStacksOwnedException(Some("Cannot both claim stack and build other stacks in the same turn.")))
    if ownedStack != None then
      if ownedStack.get.cards.size == 1 then
        ownedStack.get.clearOwner()
        ownedStack = None
    if ownedStack != None then
      var isAbleToClaim = false
      for card <- hands(playerId) do
        if !ownedStack.get.values.intersect(card.values).isEmpty then
          isAbleToClaim = true
      if !isAbleToClaim then
        return Failure(new UnableToClaimException(ownedStack.get))
    claimedCards(playerId).appendAll(cardsToClaim)
    if table.isEmpty then
      clears(playerId) += 1
    cardsToClaim.clear()
    usedCard = None
    tableBackup = table.clone()
    lastToClaimBackup = lastToClaim
    currentPlayerPos = (currentPlayerPos + 1) % numPlayers
    if hands(currentPlayerId).isEmpty then
      newHands()
    Success(())
  }
  


  private def newHands(): Unit = {
    if deckSize < numPlayers * 2 then
      endGame()
    else
      for i <- 1 to 4 do
        if deckSize >= numPlayers then
          for player <- players do
            hands(player.id).append(deck.removeHead())
      if deckSize < numPlayers * 2 && numPlayers <= deckSize then
        for player <- players do
          hands(player.id).append(deck.removeHead())
  }

  def endGame(): Unit = {
    gameFinished = true
    if lastToClaim != None then
      claimedCards(lastToClaim.get).appendAll(deck.removeAll())
    val numCards: Map[UUID, Int] = Map.empty
    val numSpades: Map[UUID, Int] = Map.empty
    scores.clear()
    var maxCards: Int = 0
    var maxSpades: Int = 0
    for player <- players do
      val playerId = player.id
      numCards(playerId) = claimedCards(playerId).size
      numSpades(playerId) = 0
      scores(playerId) = 0
      for card <- claimedCards(playerId) do
        if card.isSpades then
          numSpades(playerId) += 1
      if numCards(playerId) > maxCards then
        maxCards = numCards(playerId)
      if numSpades(playerId) > maxSpades then
        maxSpades = numSpades(playerId)
    val numWithMaxCards: Int = numCards.values.count(_ == maxCards)
    val numWithMaxSpades: Int = numSpades.values.count(_ == maxSpades)
    val report = new StringBuilder("Result of the game:\n------\n")
    def pluralS(value: BigRational): String = if value.ceil != 1 then "s" else ""
    for player <- players do
      val playerId = player.id
      report.append(s"${player.name}\n")
      // 1 point (distributed) for having most cards
      var point: BigRational = if numCards(playerId) == maxCards then BigRational(1, numWithMaxCards) else 0
      scores(playerId) += point
      report.append(s"${numCards(playerId)} card${pluralS(numCards(playerId))}: ${point} point${pluralS(point)}\n")
      // 2 points (distributed) for having most spades
      point = if numSpades(playerId) == maxSpades then BigRational(2, numWithMaxSpades) else 0
      scores(playerId) += point
      report.append(s"${numSpades(playerId)} spade${pluralS(numSpades(playerId))}: ${point} point${pluralS(point)}\n")
      // 1 point per table clears
      point = clears(playerId)
      if point > 0 then
        scores(playerId) += point
        report.append(s"${point} table clear${pluralS(point)}: ${point} point${pluralS(point)}\n")
      // 1 point for final trick
      if lastToClaim != None && playerId == lastToClaim.get then
        scores(playerId) += 1
        report.append("Last trick: 1 point\n")
      // kasino.cards worth points
      for card <- claimedCards(playerId) do
        if card.points > 0 then
          scores(playerId) += card.points
          report.append(s"${card}: ${card.points} point${pluralS(card.points)}\n")
      // total points
      report.append(s"Total points for ${player.name}: ${scores(playerId)}\n------\n")
    // Declare winner
    val maxPoints = scores.values.max
    val isDraw = (scores.values.count(_ == maxPoints) > 1)
    if !isDraw then 
      for player <- players do
        if scores(player.id) == maxPoints then
          report.append(s"The winner is ${player.name} with ${maxPoints} point${pluralS(maxPoints)}!\n")
    else
      val winners: ArrayDeque[Player] = ArrayDeque()
      for player <- players do
        if scores(player.id) == maxPoints then
          winners.append(player)
      report.append(s"Shared victory between ${winners.map(_.name).mkString(", ")} with ${maxPoints} point${pluralS(maxPoints)}!")
    resultReport = Some(report.result())
  }
  
  private def generatePlayerActions(playerId: UUID): ActionProvider = new ActionProvider {
    private def checkHasTurn(): Try[Unit] =
      if playerId != currentPlayerId then
        return Failure(new TurnOrderException(playersById(playerId), players(currentPlayerPos)))
      Success(())
    
    private def checkNoUsedCard(): Try[Unit] =
      if usedCard != None then
        return Failure(new MultipleCardsPlayedException)
      Success(())
    
    private def checkFor42(posTable: Table): Boolean = {
      if !table(posTable.i).values.contains(42) then return false
      val stack = table.remove(posTable.i) 
      cardsToClaim.appendAll(stack.cards)
      lastToClaim = Some(playerId)
      true
    }
    
    override def play(posHand: Hand): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        
        val card = Try(hands(playerId).remove(posHand.i))
        if card.isFailure then return Failure(card.failed.get)
        usedCard = Some(card.get)
        table.append(CardStack(card.get,playersById(playerId)))
        Success(())
      }
    }

    override def add(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        
        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] = {
          val usedCardCheck = checkNoUsedCard()
          if usedCardCheck.isFailure then return usedCardCheck

          val playerHand = hands(playerId)
          val result = Try((table(tablePos) + CardStack(playerHand(handPos))) (res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos, result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          checkFor42(Table(tablePos))
          Success(())
        }

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] = {
          val result = Try((table(tablePosMin) + table(tablePosMax)) (res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePosMin, result.get)
          table.remove(tablePosMax)
          checkFor42(Table(tablePosMin))
          Success(())
        }
        
        pos1 match {
          case Hand(handPos) => pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) => 
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot add a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def mod(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck

        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] = {
          val usedCardCheck = checkNoUsedCard()
          if usedCardCheck.isFailure then return usedCardCheck

          val playerHand = hands(playerId)
          val result = Try((table(tablePos) % CardStack(playerHand(handPos))) (res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos, result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          checkFor42(Table(tablePos))
          Success(())
        }

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] = {
          var result = Try((table(tablePosMin) % table(tablePosMax)) (res, Some(playersById(playerId))))
          if result.isFailure then 
            result = Try((table(tablePosMax) % table(tablePosMin)) (res, Some(playersById(playerId))))
            if result.isFailure then
              return Failure(result.failed.get)
          table.update(tablePosMin, result.get)
          table.remove(tablePosMax)
          checkFor42(Table(tablePosMin))
          Success(())
        }

        pos1 match {
          case Hand(handPos) => pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) =>
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot take remainder of a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def combine(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck

        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] = {
          val usedCardCheck = checkNoUsedCard()
          if usedCardCheck.isFailure then return usedCardCheck

          val playerHand = hands(playerId)
          val result = Try((table(tablePos) & CardStack(playerHand(handPos))) (res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos, result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          Success(())
        }

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] = {
          val result = Try((table(tablePosMin) & table(tablePosMax)) (res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePosMin, result.get)
          table.remove(tablePosMax)
          Success(())
        }

        pos1 match {
          case Hand(handPos) => pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) =>
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot combine a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def take(posTable: Table, posHand: Hand): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        
        val stack = table(posTable.i)
        val hand = hands(playerId)
        val card = hand(posHand.i)
        if stack.values.intersect(card.values).isEmpty then return Failure(new IllegalClaimException(stack,card))
        usedCard = Some(card)
        cardsToClaim.append(card)
        cardsToClaim.appendAll(stack.cards)
        lastToClaim = Some(playerId)
        hand.remove(posHand.i)
        table.remove(posTable.i)
        Success(())
      }
    }

    override def fiveOfSpades(posHand: Hand): Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        
        val hand = hands(playerId)
        val card = hand(posHand.i)
        if !card.isFiveOfSpades then return Failure(new IllegalArgumentException(card.toString + " is not the Five of Spades."))
        if table.isEmpty then return Failure(new AttemptToClearEmptyTableException)
        
        usedCard = Some(card)
        cardsToClaim.append(card)
        for stack <- table do 
          cardsToClaim.appendAll(stack.cards)
        hand.remove(posHand.i)
        table.clear()
        lastToClaim = Some(playerId)
        Success(())
      }
    }

    override def reset: Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        Try(resetTurn())
      }
    }

    override def end: Action = new Action {
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        endTurn()
      }
    }
  }
}

object Game {
  enum CardPosition {
    case Table(i: Int)
    case Hand(i: Int) 
  } 
  
  sealed trait Action {
    def apply(): Try[Unit]
  }
  
  
  sealed trait ActionProvider {
    def play(posHand : Hand): Action
    def add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Action
    def take(posTable: Table, posHand: Hand): Action
    def fiveOfSpades(posHand : Hand): Action
    def reset: Action
    def end: Action
  }
}

