package bowling

import scala.annotation.tailrec


case class GameState(val _frames: List[Frame]) {

  /**
    * Returns the list of frames of the current user
    *
    * @return List of frames that corresponds the list of frames of the current user
    */
  def frames: List[Frame] = this._frames


  /**
    * Number of frame(s) currently played by the user
    * @return Int that corresponds to the number of frame(s) currently played by the user
    */
  def nbFrames: Int = frames.size

  def getCurrentScore: Int = {
    if (frames.isEmpty) {
      0
    }
    else {
      frames.map(f => f.getScore).reduce((a,b) => a+b)
    }
  }

  def getTotalScore: Int = {
    getCurrentScore+getSumBonus(0,0)
  }

  @tailrec
  final def getSumBonus(index: Int, currentBonus: Int): Int = {
    //only when it's the last frame
    //index: from 0 to size of frames
    //the frames must have a maximum size of 13
    if (index==frames.size-1){
      currentBonus
    }
    else {
      getSumBonus(index+1, getBonusOf(index).getOrElse(0)+currentBonus)
    }

  }

  def getBonusOf(index: Int): Option[Int] = {
    frames(index).getBonusType match {
      case Util.STRIKE => {
        if (nbFrames-1-index>=2){
          Some(frames(index+1).getScore)
        }
        else if (nbFrames-1-index==1){
          Some(frames(index+1).getScore)
        }
        else {
          None
        }
      }
      case Util.SPARE => {
        if (nbFrames-1-index>=1){
          Some(frames(index+1).first)
        }
        else{ None }
      }
      case _ => None
    }
  }

  //called only when the 10th Frame is done, otherwise return -1 (by default, it returns 0 if the 10th frame is not reached yet
  def nbExtraRollAllowed: Int = {
    nbFrames match {

        //it's the last frame before the extra
      case Util.NB_NORMAL_ROUND => {
        if (frames(Util.NB_NORMAL_ROUND-1).getBonusType==Util.STRIKE){
          2
        }
        else if (frames(Util.NB_NORMAL_ROUND-1).getBonusType==Util.SPARE){
         1
        }
        else if (frames(Util.NB_NORMAL_ROUND-1).getBonusType==Util.NOTHING){
          0
        }
        else -1 //this frame is not yet completed
      }

        //the extra frame was rolled
      case (Util.NB_ROUND_PLUS_EXTRA) => 0
      //either a Util.SPARE or a Util.STRIKE

        //the last frame is not yet reached
      case _ => -1
    }
  }

  def isGameOver: Boolean = nbExtraRollAllowed==0


  def hasLost: Boolean = {
    getTotalScore<300
  }

}

object GameState{

}
