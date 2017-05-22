package com.trainologic.bowling

import org.scalatest.FlatSpec

/**
  * Created by galmarder on 08/04/2017.
  */
class BowlingSpec extends FlatSpec {

  "A bowling game" should "return 0 in gutter game" in {
    val bowlingGame = BowlingGame()
    assert(0 == bowlingGame.score().right.get)
  }

  it should "return 20 when all rolls are 1" in {
    assert(20 == rollMany(1).score().right.get)
  }

  it should "return 40 when all rolls are 2" in {
    assert(40 == rollMany(2).score().right.get)
  }

  it should "return 30 when 10 rolls are 2 and 10 rolls are 1" in {
    val bowlingGame = rollMany(2, 10)
    assert(30 == rollMany(1, 10, bowlingGame).score().right.get)
  }

  it should "add the next roll after spare as bonus" in {
    val game = spare().roll(5)
    assert(20 == rollMany(0, 17, game).score().right.get)
  }

  it should "add the next two rolls after strike as bonus" in {
    val game = strike().roll(5).roll(3)
    assert(26 == rollMany(0, 16, game).score().right.get)
  }

  it should "add another roll if last turn is spare" in {
    val game = rollMany(0, 18)
    assert(15 == spare(game).roll(5).score().right.get)
  }

  it should "add 2 additional rolls if last turn is Strike" in {
    val game = rollMany(0, 18)
    assert(16 == strike(game).roll(3).roll(3).score().right.get)
  }

  it should "return 300 for perfect game" in {
    val game = rollMany(10, 12)
    assert(300 == game.score().right.get)
  }

  it should "throw exception if rolling over 20 times and last turn wasn't spare or strike" in {
    assert(rollMany(0, 21) == InvalidBowlingGame("too much rolls"))
  }

  it should "throw exception if rolling over 21 times and last turn was spare" in {
    assert(spare(rollMany(0, 18)).roll(0).roll(0) == InvalidBowlingGame("too much rolls"))
  }

  it should "throw exception if rolling over 22 times and last turn was strike" in {
    assert(strike(rollMany(0, 18)).roll(0).roll(0).roll(0) == InvalidBowlingGame("too much rolls"))
  }

  it should "throw exception if rolling over 22 times with strikesand last turn was strike" in {
    assert(strike(strike(strike(strike(rollMany(0, 18))))) == InvalidBowlingGame("too much rolls"))
  }

  def strike(game: BowlingGame = BowlingGame()):BowlingGame = {
    game.roll(10)
  }

  def spare(game: BowlingGame = BowlingGame()):BowlingGame = {
    rollMany(5, 2, game)
  }


  def rollMany(pins:Int, times:Int = 20, bowlingGame:BowlingGame = BowlingGame()): BowlingGame = {
    (1 to times).foldLeft(bowlingGame)((game, _) => game.roll(pins))
  }
}