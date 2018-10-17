package bowling

object Bowling extends App {


  val player: GameState = GameState(_frames = Nil)

  mainLoop(player)

  def mainLoop(player: GameState): Unit = {
      player.nbExtraRollAllowed match {
        case 0 => {
          ioHandler.printScore(player.getTotalScore)
        }
        case 1 => {
          ioHandler.printExtraFromSpare
          val roll1: Int = ioHandler.askForInput
          mainLoop(player.copy(_frames = player.frames :+ Frame(roll1, Util.GUTTER)))
        }
        case 2 => {
          //2 extras because strike
          ioHandler.printAskForInput(1, player.nbFrames + 1)
          val roll1: Int = ioHandler.askForInput
          ioHandler.printAskForInput(2, player.nbFrames + 1)
          val roll2: Option[Int] = Some(ioHandler.askForInput)
          mainLoop(player.copy(_frames = player.frames :+ Frame(roll1, roll2)))
        }
        case _ => {
          if (player.frames.nonEmpty && player.frames(player.nbFrames - 1).getBonusType != Util.NOTHING_YET) {
            //it is Completed so ask for the next frame and -> add None to the second roll
          ioHandler.printAskForInput(1, player.nbFrames + 1)
            val roll1: Int = ioHandler.askForInput
            val p: GameState = player.copy(_frames = player.frames :+ Frame(roll1, Util.GUTTER))
            println(p)
            roll1==10 match {
              case true => mainLoop(player.copy(_frames = player.frames :+ Frame(roll1, Util.GUTTER)))
              case _ => mainLoop(player.copy(_frames = player.frames :+ Frame(roll1, None)))
            }
          }
          else {
            //either the not yet reached the 10th frame or 10th frame not yet completed
            if(player.frames.isEmpty){
              ioHandler.printAskForInput(1, player.nbFrames)
            }
            else{
              ioHandler.printAskForInput(2, player.nbFrames)
            }
            val roll2: Option[Int] = Some(ioHandler.askForInput)
            val newFrame: Frame = player.frames(player.frames.size - 1).copy(_second = roll2)
            mainLoop(player.copy(player.frames.dropRight(1) :+ newFrame))
          }
        }
      }
    }
}


