package bowling

case class Frame(val _first: Int, val _second: Option[Int]) {

  /**
    * Returns the first pin of the frame
    *
    * @return Int that corresponds to the first pin of the frame
    */
  def first: Int = this._first

  /**
    * Returns the second pin of the frame
    *
    * @return Int that corresponds to the second pin of the frame
    */
  def second: Option[Int] = this._second

  /**
    * Returns the type of the bonus of this frame, either:
    * Util.STRIKE: if a strike have been rolled;
    * Util.SPARE: if a spare have been rolled;
    * Util.NOTHING: if a neither a spare nor a strike have been rolled;
    * Util.NOTHING_YET: if a a strike was not rolled in the current frame;
    * @return String that corresponds to type of the bonus of acquired for this frame.
    */
  def getBonusType: String = {
    first match {
      case 10 => Util.STRIKE
      case _ => {
        if (first+second.getOrElse(0) ==10){
          Util.SPARE
        }
        else if (second.isEmpty) Util.NOTHING_YET else Util.NOTHING
      }
    }
  }

  /**
    * Gives the score gained by this single frame
    * @return Int that corresponds to the score gained by his frame.
    */
  def getScore: Int = {
    first match {
      case 10 => 10
      case _ => first+second.getOrElse(0)
    }
  }
}
