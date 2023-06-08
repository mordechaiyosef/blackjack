// command line blackjack
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.annotation.tailrec


def getScore: Map[String, Int] = {
  Map("Ace" -> 11, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5,
    "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9, "10" -> 10,
    "Jack" -> 10, "Queen" -> 10, "King" -> 10)
}

var SCORE_MAP = getScore
val BALANCE_FILENAME = "balance.json"

class Hand {
  private val cards: ListBuffer[String] = ListBuffer()

  def showHand(): Unit = {
    for (card <- cards) {
      println(card)
    }
  }
  def addCard(card: String): Unit = {
    cards += card
  }

  def getScore: Int = {
    var score = 0
    for (card <- cards) {
      // get first word in card
      val number = card.split(" ")(0)
      if (number == "Ace") {
        if (score + 11 > 21) {
          score += 1
        } else {
          score += 11
        }
      } else {
        val cardScore = SCORE_MAP(number)
        score += cardScore
      }
    }
    score
  }

}

def getSuits: List[String] = {
  List("Hearts", "Diamonds", "Spades", "Clubs")
}
def getRanks: List[String] = {
  List("Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                   "Jack", "Queen", "King")
}

def newHand(deck: ListBuffer[String]): Hand = {
  val hand = new Hand
  hand.addCard(dealCard(deck))
  hand.addCard(dealCard(deck))
  hand
}


def makeDeck: ListBuffer[String]  = {
  val cards: ListBuffer[String] = ListBuffer()
  for (suit <- getSuits) {
    for (rank <- getRanks) {
      val score = SCORE_MAP(rank)
      cards += (rank + " of " + suit + " (" + score + ")")
    }
  }
  cards
}


def shuffleDeck(cards: ListBuffer[String]): ListBuffer[String] = {
  // get keys
  // shuffle keys
  scala.util.Random.shuffle(cards)
}

def dealCard(cards: ListBuffer[String]): String = {
  val card = cards.head
  cards.remove(0)
  card
}

@tailrec
def dealerAction(deck: ListBuffer[String], dealerHand: Hand): Unit = {
  if (dealerHand.getScore < 17) {
    dealerHand.addCard(dealCard(deck))
    dealerAction(deck, dealerHand)
  }
}

def getFinalScore(playerHand: Hand, dealerHand: Hand): Unit = {
  println("Player's hand:")
  playerHand.showHand()
  println("\n")
  println("Dealer's hand:")
  dealerHand.showHand()
  println("\n")
  println("Player's score: " + playerHand.getScore)
  println("Dealer's score: " + dealerHand.getScore)
  println("\n")
  if (dealerHand.getScore > 21) {
    println("Dealer busts!")
  } else if (dealerHand.getScore > playerHand.getScore) {
    println("Dealer wins!")
  } else if (dealerHand.getScore == playerHand.getScore) {
    println("Push! (tie)")
  } else {
    println("Player wins!")
  }
}


@tailrec
def actionLoop(deck: ListBuffer[String], playerHand: Hand, dealerHand: Hand): Unit = {
  println("\naction: hit|stand \n")
  val action = readLine()
  if (action == "hit") {
    playerHand.addCard(dealCard(deck))
    println("\nPlayer's hand:")
    playerHand.showHand()
    print("")
    if (playerHand.getScore > 21) {
      println("\nBust!")
    } else {
      actionLoop(deck, playerHand, dealerHand)
    }
  } else if (action == "stand") {
    dealerAction(deck, dealerHand)
    getFinalScore(playerHand, dealerHand)
  } else {
    println("invalid action\n")
    actionLoop(deck, playerHand, dealerHand)
  }
}

def getBalance(username: String): Int = {
  // get balance from json file
  val jsonData =scala.io.Source.fromFile(BALANCE_FILENAME)
  val json = jsonData.mkString
  jsonData.close()
  100

  // if file doesn't exist, create file

  // return balance
}

def validateUsername(username: String): Boolean = {
  // check if has special characters
  if (username.forall(_.isLetterOrDigit)) {
    return true
  }
  false
}

def startGame(): Unit = {
  val deck = makeDeck
  val shuffledDeck = shuffleDeck(deck)
  val dealerHand = newHand(shuffledDeck)
  val playerHand = newHand(shuffledDeck)
  println("Lets Play!\n")
  println("Player's hand:")
  playerHand.showHand()
  actionLoop(shuffledDeck, playerHand, dealerHand)
}

def getUsername: String = {
  println("write a valid username (letters and digits only):")
  val username = readLine()
  if (!validateUsername(username)) {
    println("invalid username")
    getUsername
  }
  username
}

def main(): Unit = {
  println("Welcome to Blackjack!\n")
  val username = getUsername
  println("Hello " + username + "!")
  println("Do you want to play? (y/n)")
  val answer = readLine()
  if (answer == "y") {
    startGame()
  } else {
    println("Goodbye!")
  }
}

@main def x :Unit = main()
