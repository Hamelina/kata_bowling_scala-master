package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {
  describe("Bowling score") {

    it("should be 0 when all roll into gutter => all the rolls equal to 0"){
      //both 1st and 2nd try are equal to 0
      Frame(0,Some(0)).getScore should be (0)

    }

    it("should be 10 if the first try is 10"){
      //here is the score without bonus
      Frame(10,Some(0)).getScore should be (10)
      Frame(5, Some(5)).getScore should be (10)

    }

    it("Should be 10 if the first try + the second try is 10 otherwise it should be the sum of the 2 rolls"){
      //it is a Util.SPARE
      Frame(5,Some(5)).getScore should be (10)
      Frame(2,Some(7)).getScore should be (9)
      Frame(7,Some(0)).getScore should be (7)
    }

  }

  describe("Frame bonus") {

    it("Should be Util.STRIKE when the first try is 10"){
      //both 1st and 2nd try are equal to 0
      Frame(10,None).getBonusType should be (Util.STRIKE)
      //Frame(9,0).getBonusType should not be ("Util.STRI ")

    }

    it("Should be Util.SPARE if the first try + the second try is 10"){
      //here is the score without bonus
      Frame(5,Some(5)).getBonusType should be (Util.SPARE)
    }

    it("should be Util.NOTHING if the first try + the second try is less than 10"){
      Frame(2,Some(7)).getBonusType should be (Util.NOTHING)
    }
    it("should be Util.NOTHING if it is not a strike and the next roll have not been done yet"){
      Frame(2,None).getBonusType should be (Util.NOTHING_YET)
    }
  }


  val gameState1 = GameState(List(Frame(10,None), Frame(7,Some(0))))
  val gameStateStrike = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None)))
  val gameStateSpare = GameState(List(Frame(10,None), Frame(5,Some(5)), Frame(7,Some(0))))
  val gameStateSpareStrike = GameState(List(Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)), Frame(5,Some(5))))

  val gameNotFinishedYet = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None)))
  val gameFinishedForGood = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0))))
  val gameFinishedWithSpare = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(5,Some(5))))
  val gameFinishedWithStrike = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(10,None)))
  val gameFinishedWithSpareAfter1Roll = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(5,Some(5)), Frame(5, None)))
  val gameFinishedWithStrikeAfter1Roll = GameState(List(Frame(10,None), Frame(7,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(7,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(10,None), Frame(10, Some(10))))
  val gameWon = GameState(List(Frame(10,None), Frame(10,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(10,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(10,Some(0))))
  val gameWonWithFullScore = GameState(List(Frame(10,None), Frame(10,Some(0)), Frame(10,None), Frame(10,None), Frame(10,None), Frame(10,Some(0)),Frame(10,None), Frame(10,None), Frame(10,None), Frame(10,Some(0)), Frame(10,Some(0)), Frame(10,Some(0))))


  describe(" Game Sum Bonus "){
    it("Should be bonus of each frame"){
      gameState1.getBonusOf(0) should be (Some(7))
      gameStateStrike.getBonusOf(0) should be (Some(7))
      gameStateSpare.getBonusOf(1) should be (Some(7))
      gameStateSpareStrike.getBonusOf(0) should be (Some(10))
      gameStateSpareStrike.getBonusOf(3) should be (None)
      gameStateSpareStrike.getBonusOf(2) should be (Some(7))
      gameStateSpareStrike.getBonusOf(4) should be (None)
      gameFinishedWithStrikeAfter1Roll.getBonusOf(10) should be (None)
    }

    it("should be the sum of the bonuses"){
      gameState1.getSumBonus(0,0) should be (7)
      gameStateStrike.getSumBonus(0,0) should be (7)
      gameStateSpare.getSumBonus(0,0) should be (17)
      gameStateSpareStrike.getSumBonus(0,0) should be (27)
    }

  }


  describe("Game score") {

    it("Should be the size of the current list of frames"){
      gameState1.nbFrames should be (2)

    }
    it("Should be the total score (doesn't take into account the bonuses"){
      gameState1.getCurrentScore should be (17)
      gameStateStrike.getCurrentScore should be (27)
      gameStateSpare.getCurrentScore should be (27)
      gameStateSpareStrike.getCurrentScore should be (47)
    }

    it("Should be the total score (takes into account the bonuses"){
      gameState1.getTotalScore should be (24)
      gameStateStrike.getTotalScore should be (34)
      gameStateSpare.getTotalScore should be (44)
      gameStateSpareStrike.getTotalScore should be (74)
    }

  }



  describe("Game Allowed to play again"){

    it("Should be -1 because the 10 frames are not all done yet"){
      gameNotFinishedYet.nbExtraRollAllowed should be (-1)
    }

    it("Should be 2 because the number the 10th frame is a Util.STRIKE"){
      gameFinishedWithStrike.nbExtraRollAllowed should be (2)
    }

    it("Should be 1 because the number the 10th frame is a Util.SPARE"){
      gameFinishedWithSpare.nbExtraRollAllowed should be (1)
    }

    it("Should be 0 because the number the 10th frame is a Util.SPARE and the 1st rool of the 11th frame has been done already "){
      gameFinishedWithSpareAfter1Roll.nbExtraRollAllowed should be (0)
    }

  }

  describe("Game Over"){

    //isGameOver
    it("Should be true because the game is over"){
      gameFinishedForGood.isGameOver should be (true)
      gameFinishedWithSpareAfter1Roll.isGameOver should be (true)
      gameFinishedWithStrikeAfter1Roll.isGameOver should be (true)
    }

    //isGameOver
    it("Should be false because the game is not over"){
      gameFinishedWithSpare.isGameOver should be (false)
      gameNotFinishedYet.isGameOver should be (false)
      gameFinishedWithSpare.isGameOver should be (false)

    }

  }

}
