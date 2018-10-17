package bowling

object Bowling extends App {


  val player: GameState = GameState(_frames = Nil)

  mainLoop(player)

  def mainLoop(player: GameState): Unit = {
      player.nbExtraRollAllowed match {
        case 0 => {
          //no more frame to play
          ioHandler.printScore(player.getTotalScore)
        }
        case 1 => {
          //1 extra frame to play because 10th frame rolled a spare
          ioHandler.printExtraFromSpare
          val roll1: Int = ioHandler.askForInput
          mainLoop(player.copy(_frames = player.frames :+ Frame(roll1, Util.GUTTER)))
        }
        case 2 => {
          //2 extra frame to play because 10th frame rolled a strike
          ioHandler.printAskForInput(1, player.nbFrames + 1)
          val roll1: Int = ioHandler.askForInput
          ioHandler.printAskForInput(2, player.nbFrames + 1)
          val roll2: Option[Int] = Some(ioHandler.askForInput)
          mainLoop(player.copy(_frames = player.frames :+ Frame(roll1, roll2)))
        }
        case _ => {
          //game not yet finished and did not yet reached the 10th frame.
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
              val roll1: Int = ioHandler.askForInput
              roll1==10 match {
                case true => mainLoop(player.copy(_frames = Frame(roll1, Util.GUTTER)::Nil))
                case _ => mainLoop(player.copy(_frames = Frame(roll1, None)::Nil))
              }
            }
            else{
              ioHandler.printAskForInput(2, player.nbFrames)
            }
            val roll2: Option[Int] = Some(ioHandler.askForInput)
            val newFrame: Frame = player.frames(player.nbFrames).copy(_second = roll2)
            //val p2: List[Frame] = player.frames.dropRight(1) :+ newFrame
            mainLoop(player.copy(player.frames.dropRight(1) :+ newFrame))
          }
        }
      }
    }
}


