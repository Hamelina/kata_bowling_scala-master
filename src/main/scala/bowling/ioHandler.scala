package bowling

import scala.io.StdIn.readLine

object ioHandler {

  //input
  def askForInput: Int = {
    val rollString = readLine
    val roll: Int = rollString.toInt
    if (roll<0 || roll>10){
      askForInput
    }
    else roll
  }




  //output
  def printAskForInput(numRoll: Int, frame: Int): Unit ={
    print(s" Enter your roll $numRoll of frame $frame: \n")
  }

  def printScore(score: Int): Unit ={
    print(s"Your final score is: $score \n")
  }

  def printExtraFromSpare: Unit = {
    print("Well done, rolled a spare ! You have the chance to roll once again because.")
  }

  def printExtraFromStrike: Unit = {
    print("Well done, rolled a strike ! You have the chance to roll twice again because.")
  }

}
