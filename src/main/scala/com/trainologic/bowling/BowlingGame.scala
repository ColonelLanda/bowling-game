package com.trainologic.bowling

/**
  * Created by alexlanda on 23/04/2017.
  */
case class ValidBowlingGame(frames: List[Frame]) extends BowlingGame {
  type Validation = Int => Option[String]

  private def condToOption(cond: => Boolean, msg: String) = if (cond) Some(msg) else None

  private val validations: Seq[Validation]= Seq(
    p => condToOption(p < 0, s"$p is invalid roll"),
    p => condToOption(p > 10, s"$p is invalid roll"),
    p => frames.headOption.collect {
      case Frame(roll1, None) if roll1 < 10 && p + roll1 > 10 => s"$p is invalid roll"
    },
    _ => condToOption(frames.length == 10 && (frames.head.completed && frames.head.rolls.sum < 10) ,"too much rolls"),
    _ => condToOption(frames.length == 11 && (!frames.head.completed && !frames.tail.head.isStrike),"too much rolls"),
    _ => condToOption(frames.length == 11 && (frames.head.completed && !frames.head.isStrike) ,"too much rolls"),
    _ => condToOption(frames.length == 12 && frames.head.completed ,"too much rolls")
  )

  def roll(pins: Int): BowlingGame = {
    validations.view.map(_ (pins)).collectFirst {case Some(err) => InvalidBowlingGame(err)}.getOrElse {
      frames match {
        case Nil => ValidBowlingGame(List(Frame(pins, None)))
        case lastRole :: _ if lastRole.completed => ValidBowlingGame(Frame(pins, None) :: frames)
        case Frame(roll1, None) :: others if (10 - roll1) >= pins => ValidBowlingGame(Frame(roll1, Some(pins)) :: others)
      }
    }
  }

  def score(): Either[String, Int] = {
    Right {
      val orderedFrames = frames.reverse ++  (1 to (12 - frames.length)).map(_ => Frame(0,None))
      orderedFrames.sliding(3).foldLeft(0) {
        case (score, Frame(10, _) :: tail) => score + 10 + tail.flatMap(_.rolls).take(2).sum
        case (score, List(frame1, frame2, _)) =>
          score + frame1.score + (if (frame1.isSpare) frame2.roll1 else 0)
      }
    }
  }
}

case class Frame(roll1: Int, roll2: Option[Int]) {
  def isStrike: Boolean = roll1 == 10

  def isSpare: Boolean = !isStrike && roll2.exists(_ + roll1 == 10)

  def completed: Boolean = isStrike || roll2.isDefined

  def rolls: Seq[Int] = Seq(roll1) ++ roll2.toSeq

  def score: Int = rolls.sum

}

object BowlingGame {
  def apply(): BowlingGame = ValidBowlingGame(List())
}

trait BowlingGame {
  def roll(pins: Int): BowlingGame

  def score(): Either[String, Int]
}


case class InvalidBowlingGame(errMsg: String) extends BowlingGame {
  override def roll(pins: Int): BowlingGame = this

  override def score(): Either[String, Int] = Left(errMsg)
}

