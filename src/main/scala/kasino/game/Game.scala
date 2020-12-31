package kasino.game

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import kasino.akka.{Dispatch, KasinoActor, fetch}
import kasino.cards.Card
import kasino.exceptions.{AttemptToClearEmptyTableException, IllegalClaimException, KasinoException, MultipleCardsPlayedException, MultipleStacksOwnedException, NoCardsPlayedException, TurnOrderException, UnableToClaimException}
import kasino.game.Game.{ActionProvider, CardPosition}
import kasino.game.Game.CardPosition._
import reeh.math.BigRational

import java.util.UUID
import scala.collection.mutable.{ArrayDeque, Map, Queue}
import scala.util.{Failure, Success, Try}

object Game {
  def apply(parent: ActorRef[kasino.MainActor.Message], controllers: Iterable[ActorRef[Dispatch[Controller.Message]]], newDeck: Iterable[Card]): Behavior[Dispatch[Message]] = Behaviors.setup(context => new Game(parent, controllers, newDeck, context))
  
  enum Message extends kasino.akka.Message() {
    case Run()
    case Act(action: Action)
    case GetGameFinished(replyTo: ActorRef[Boolean])
    case GetResultReport(replyTo: ActorRef[Option[String]])
  }
  
  enum CardPosition {
    case Table(i: Int)
    case Hand(i: Int)
  }
  
  sealed trait Action {
    private[Game] def apply(): Try[Unit]

    val playerId: UUID
  }
  
  object Action {
    abstract class Play(val posHand: Hand) extends Action
    abstract class Add(val pos1: CardPosition, val pos2: CardPosition, val res: Option[Int] = None) extends Action
    abstract class Mod(val pos1: CardPosition, val pos2: CardPosition, val res: Option[Int] = None) extends Action
    abstract class Combine(val pos1: CardPosition, val pos2: CardPosition, val res: Option[Int] = None) extends Action
    abstract class Take(val posTable: Table, val posHand: Hand) extends Action
    abstract class FiveOfSpades(val posHand : Hand) extends Action
    abstract class Reset extends Action
    abstract class End extends Action
  }
  

  sealed trait ActionProvider {
    import Action._
    
    def play(posHand : Hand): Play
    def add(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Add
    def mod(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Mod
    def combine(pos1: CardPosition, pos2: CardPosition, res: Option[Int] = None): Combine
    def take(posTable: Table, posHand: Hand): Take
    def fiveOfSpades(posHand : Hand): FiveOfSpades
    def reset: Reset
    def end: End
  }
}


/**
 * A concrete game of "Nørdekasino".
 * 
 * @param controllers controllers for the [[Player]]s in this game.
 * @param newDeck a deck of [[kasino.cards.Card]]s with which to play a game. Needs at least `4* controllers.size +2` cards.
 */
class Game (parent: ActorRef[kasino.MainActor.Message], controllers: Iterable[ActorRef[Dispatch[Controller.Message]]], newDeck: Iterable[Card], override implicit val context: ActorContext[Dispatch[Game.Message]]) extends KasinoActor[Game.Message] {
  require(newDeck.size >= 4 * controllers.size + 2, "The provided deck is too small for a game with " + controllers.size + " players.")

  private class Deck (deck: Iterable[Card]) extends Queue[Card](deck.size) {
    this.appendAll(deck)

    def draw(): Card = removeHead()
  }

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

  //Primary game state
  private val deck: Deck = new Deck(scala.util.Random.shuffle(newDeck))
  private val table: ArrayDeque[CardStack] = ArrayDeque()
  private val hands: Map[UUID, ArrayDeque[Card]] = Map.empty
  private val playersById: Map[UUID, ActorRef[Dispatch[Player.Message]]] = Map.empty
  private val players: scala.collection.immutable.Seq[ActorRef[Dispatch[Player.Message]]] =
    scala.util.Random.shuffle(controllers).map{c =>
      val hand: ArrayDeque[Card] = ArrayDeque()
      val playerId: UUID = fetch(c, Controller.Message.GetId(_))
      val player: ActorRef[Dispatch[Player.Message]] = context.spawn(Behaviors.setup(context => new Player(c, hand.view, table.view, deckSize, currentPlayerId, currentPlayerName, generatePlayerActions(playerId), this.context.self, context)), "Player-"+playerId)
      hands.addOne(playerId, hand)
      playersById.addOne(playerId, player)
      claimedCards.addOne(playerId, ArrayDeque())
      clears.addOne(playerId, 0)
      player
    }.toSeq
  private val numPlayers = players.size


  /** The number of [[kasino.cards.Card]]s remaining in the deck. */
  def deckSize : Int = deck.size

  /** The [[java.util.UUID]] of the current [[Player]]. */
  def currentPlayerId: UUID = fetch(players(currentPlayerPos), Player.Message.GetId(_))

  /** The name of the current [[Player]]. */
  def currentPlayerName: String = fetch(players(currentPlayerPos), Player.Message.GetName(_))

  override def actOnMessage(message: Game.Message): KasinoActor[Game.Message] = {
    import Game.Message._
    message match {
      case Run() => run()
      case GetGameFinished(replyTo: ActorRef[Boolean]) => replyTo ! gameFinished
      case Act(action: Game.Action) => {
        val result = action()
        sendMessage(playersById(action.playerId), Player.Message.ActionResult(action, result))
        action match {
          case _: Game.Action.End if (result.isSuccess && !gameFinished) => sendMessage(playersById(currentPlayerId), Player.Message.TakeTurn())
          case _: Game.Action.End if (result.isSuccess && gameFinished) => parent ! kasino.MainActor.Message.GameFinished
          case _ => ()
        }
      }
      case GetResultReport(replyTo: ActorRef[Option[String]]) => replyTo ! resultReport
    }
    this
  }
  
  def setup(): Try[Unit] = {
    if gameStarted then
      return Failure(new RuntimeException("Game already running."))
    for player <- players do
      for i <- 1 to 4 do
        hands(fetch(player, Player.Message.GetId(_))).append(deck.draw())
    for i <- 1 to 2 do
      table.append(CardStack(deck.draw()))
    currentPlayerPos = 0
    usedCard = None
    lastToClaim = None
    lastToClaimBackup = None
    cardsToClaim.clear()
    tableBackup = table.clone()
    postGameState()
    gameStarted = true
    return Success(())
  }

  def run(): Unit = {
    setup()
    sendMessage(players(currentPlayerPos), Player.Message.TakeTurn())
  }

  private def postGameState(): Unit = {
    for player <- players do
      sendMessage(player, Player.Message.UpdateGameState())
  }

  private def resetTurn(): Unit = {
    table.clear()
    table.appendAll(tableBackup)
    for card <- usedCard do
      hands(currentPlayerId).append(card)
    usedCard = None
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
      if stack.ownerId == Some(playerId) then
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
            hands(fetch(player, Player.Message.GetId(_))).append(deck.draw())
      if deckSize < numPlayers * 2 && numPlayers <= deckSize then
        for player <- players do
          hands(fetch(player, Player.Message.GetId(_))).append(deck.draw())
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
      val playerId: UUID = fetch(player, Player.Message.GetId(_))
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
      val playerId: UUID = fetch(player, Player.Message.GetId(_))
      val playerName: String = fetch(player, Player.Message.GetName(_))
      report ++= s"${playerName}\n"
      // 1 point (distributed) for having most cards
      var point: BigRational = if numCards(playerId) == maxCards then BigRational(1, numWithMaxCards) else 0
      scores(playerId) += point
      report ++= s"${numCards(playerId)} card${pluralS(numCards(playerId))}: ${point} point${pluralS(point)}\n"
      // 2 points (distributed) for having most spades
      point = if numSpades(playerId) == maxSpades then BigRational(2, numWithMaxSpades) else 0
      scores(playerId) += point
      report ++= s"${numSpades(playerId)} spade${pluralS(numSpades(playerId))}: ${point} point${pluralS(point)}\n"
      // 1 point per table clears
      point = clears(playerId)
      if point > 0 then
        scores(playerId) += point
        report ++= s"${point} table clear${pluralS(point)}: ${point} point${pluralS(point)}\n"
      // 1 point for final trick
      if lastToClaim != None && playerId == lastToClaim.get then
        scores(playerId) += 1
        report ++= "Last trick: 1 point\n"
      // kasino.cards worth points
      for card <- claimedCards(playerId) do
        if card.points > 0 then
          scores(playerId) += card.points
          report ++= s"${card}: ${card.points} point${pluralS(card.points)}\n"
      // total points
      report ++= s"Total points for ${playerName}: ${scores(playerId)}\n------\n"
    // Declare winner
    val maxPoints = scores.values.max
    val isDraw = (scores.values.count(_ == maxPoints) > 1)
    if !isDraw then
      for player <- players do
        if scores(fetch(player, Player.Message.GetId(_))) == maxPoints then
          report ++= s"The winner is ${fetch(player, Player.Message.GetName(_))} with ${maxPoints} point${pluralS(maxPoints)}!\n"
    else
      val winners: ArrayDeque[ActorRef[Dispatch[Player.Message]]] = ArrayDeque()
      for player <- players do
        if scores(fetch(player, Player.Message.GetId(_))) == maxPoints then
          winners.append(player)
      report ++= s"Shared victory between ${winners.map(fetch(_, Player.Message.GetName(_))).mkString(", ")} with ${maxPoints} point${pluralS(maxPoints)}!"
    resultReport = Some(report.result())
  }
  
  private def generatePlayerActions(newPlayerId: UUID): ActionProvider = new ActionProvider {
    import Game.Action._
    
    private def checkHasTurn(): Try[Unit] =
      if newPlayerId != currentPlayerId then
        return Failure(new TurnOrderException(playersById(newPlayerId), players(currentPlayerPos)))
      Success(())
    
    private def checkNoUsedCard(): Try[Unit] =
      if usedCard != None then
        return Failure(new MultipleCardsPlayedException)
      Success(())
    
    private def checkFor42(posTable: Table): Boolean = {
      if !table(posTable.i).values.contains(42) then return false
      val stack = table.remove(posTable.i) 
      cardsToClaim.appendAll(stack.cards)
      lastToClaim = Some(newPlayerId)
      true
    }
    
    override def play(posHand: Hand): Play = new Play(posHand) {
      override val playerId: UUID = newPlayerId
      
      override def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        
        val card = Try(hands(playerId).remove(this.posHand.i))
        if card.isFailure then return Failure(card.failed.get)
        usedCard = Some(card.get)
        table.append(CardStack(card.get,playersById(playerId)))
        postGameState()
        Success(())
      }
    }

    override def add(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Add = new Add(pos1, pos2, res) {
      override val playerId: UUID = newPlayerId
      
      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        
        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] = {
          val usedCardCheck = checkNoUsedCard()
          if usedCardCheck.isFailure then return usedCardCheck

          val playerHand = hands(playerId)
          val result = Try((table(tablePos) + CardStack(playerHand(handPos))) (this.res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos, result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          checkFor42(Table(tablePos))
          postGameState()
          Success(())
        }

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] = {
          val result = Try((table(tablePosMin) + table(tablePosMax)) (this.res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePosMin, result.get)
          table.remove(tablePosMax)
          checkFor42(Table(tablePosMin))
          postGameState()
          Success(())
        }

        this.pos1 match {
          case Hand(handPos) => this.pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => this.pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) => 
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot add a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def mod(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Mod = new Mod(pos1, pos2, res) {
      override val playerId: UUID = newPlayerId

      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck

        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] = {
          val usedCardCheck = checkNoUsedCard()
          if usedCardCheck.isFailure then return usedCardCheck

          val playerHand = hands(playerId)
          val result = Try((table(tablePos) % CardStack(playerHand(handPos))) (this.res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos, result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          checkFor42(Table(tablePos))
          postGameState()
          Success(())
        }

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] = {
          var result = {
            // table(tablePosMin) comes directly from the player hand this turn
            if table(tablePosMin).cards.size == 1 && table(tablePosMin).ownerId == Some(playerId) then
              Try((table(tablePosMax) % table(tablePosMin)) (this.res, Some(playersById(playerId))))
            // table(tablePosMax) comes directly from the player hand this turn
            else if table(tablePosMax).cards.size == 1 && table(tablePosMax).ownerId == Some(playerId) then
              Try((table(tablePosMin) % table(tablePosMax)) (this.res, Some(playersById(playerId))))
            // both CardStacks are pre-existing on the table
            else
              var biResult = Try((table(tablePosMin) % table(tablePosMax)) (this.res, Some(playersById(playerId))))
              if biResult.isFailure then
                biResult = Try((table(tablePosMax) % table(tablePosMin)) (this.res, Some(playersById(playerId))))
              biResult
          }
          if result.isFailure then
            return Failure(result.failed.get)
          table.update(tablePosMin, result.get)
          table.remove(tablePosMax)
          checkFor42(Table(tablePosMin))
          postGameState()
          Success(())
        }

        this.pos1 match {
          case Hand(handPos) => this.pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => this.pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) =>
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot take remainder of a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def combine(pos1: Game.CardPosition, pos2: Game.CardPosition, res: Option[Int]): Combine = new Combine(pos1, pos2, res) {
      override val playerId: UUID = newPlayerId

      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck

        def tableAndHand (tablePos: Int, handPos: Int): Try[Unit] = {
          val usedCardCheck = checkNoUsedCard()
          if usedCardCheck.isFailure then return usedCardCheck

          val playerHand = hands(playerId)
          val result = Try((table(tablePos) & CardStack(playerHand(handPos))) (this.res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePos, result.get)
          usedCard = Some(playerHand(handPos))
          playerHand.remove(handPos)
          postGameState()
          Success(())
        }

        def tableAndTable (tablePosMin: Int, tablePosMax: Int): Try[Unit] = {
          val result = Try((table(tablePosMin) & table(tablePosMax)) (this.res, Some(playersById(playerId))))
          if result.isFailure then return Failure(result.failed.get)
          table.update(tablePosMin, result.get)
          table.remove(tablePosMax)
          postGameState()
          Success(())
        }

        this.pos1 match {
          case Hand(handPos) => this.pos2 match {
            case Hand(_) => return Failure(new MultipleCardsPlayedException)
            case Table(tablePos) => tableAndHand(tablePos, handPos)
          }
          case Table(tablePos1) => this.pos2 match {
            case Hand(handPos) => tableAndHand(tablePos1, handPos)
            case Table(tablePos2) =>
              if tablePos1 == tablePos2 then return Failure(new IllegalArgumentException("Cannot combine a CardStack with itself."))
              tableAndTable(tablePos1 min tablePos2, tablePos1 max tablePos2)
          }
        }
      }
    }

    override def take(posTable: Table, posHand: Hand): Take = new Take(posTable, posHand) {
      override val playerId: UUID = newPlayerId

      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck

        val stack = Try(table(this.posTable.i))
        if stack.isFailure then return Failure(stack.failed.get)
        val hand = hands(playerId)
        val card = Try(hand(this.posHand.i))
        if card.isFailure then return Failure(card.failed.get)
        
        if stack.get.values.intersect(card.get.values).isEmpty then return Failure(new IllegalClaimException(stack.get,card.get))
        usedCard = Some(card.get)
        cardsToClaim.append(card.get)
        cardsToClaim.appendAll(stack.get.cards)
        lastToClaim = Some(playerId)
        hand.remove(this.posHand.i)
        table.remove(this.posTable.i)
        postGameState()
        Success(())
      }
    }

    override def fiveOfSpades(posHand: Hand): FiveOfSpades = new FiveOfSpades(posHand) {
      override val playerId: UUID = newPlayerId

      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val usedCardCheck = checkNoUsedCard()
        if usedCardCheck.isFailure then return usedCardCheck
        
        val hand = hands(playerId)
        val card = Try(hand(this.posHand.i))
        if card.isFailure then return Failure(card.failed.get)


        if !card.get.isFiveOfSpades then return Failure(new IllegalArgumentException(card.get.toString + " is not the Five of Spades."))
        if table.isEmpty then return Failure(new AttemptToClearEmptyTableException)
        
        usedCard = Some(card.get)
        cardsToClaim.append(card.get)
        for stack <- table do 
          cardsToClaim.appendAll(stack.cards)
        hand.remove(this.posHand.i)
        table.clear()
        lastToClaim = Some(playerId)
        postGameState()
        Success(())
      }
    }

    override def reset: Reset = new Reset {
      override val playerId: UUID = newPlayerId

      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val attempt = Try(resetTurn())
        if attempt.isSuccess then
          postGameState()
        attempt
      }
    }

    override def end: End = new End {
      override val playerId: UUID = newPlayerId

      def apply(): Try[Unit] = {
        val turnCheck = checkHasTurn()
        if turnCheck.isFailure then return turnCheck
        val attempt = endTurn()
        if attempt.isSuccess then
          postGameState()
        attempt
      }
    }
  }
}


