package io.github.nwtgck.othello.player

import io.github.nwtgck.othello.{Board, Disk, Empty, Position}

/**
  * Player based on evaluation-table
  * @param disk
  * @param table
  * @tparam D
  */
class EvaluationTablePlayer[D <: Disk](override val disk: D, table: Seq[Seq[Int]]) extends Player(disk) {

  // Position to Evaluation
  private[this] val posToEvaluation: Map[Position, Int] =
    (for {
      i <- 0 until Board.Size
      j <- 0 until Board.Size
    } yield (Position(i, j), table(i)(j))).toMap

  override def move(board: Board): Position = {
    board.movablePositions(disk).maxBy{movedPos =>
      // Get moved board
      val movedBoard = board.moved(disk, movedPos)

      // Sum all evaluation values
      (for {
        i <- 0 until Board.Size
        j <- 0 until Board.Size
      } yield {
        val pos = Position(i, j)
        val cell = movedBoard(pos)
        posToEvaluation(pos) * (cell match {
          case Empty  => 0
          case `disk` => 1
          case _      => -1
        })
      }).sum
    }

  }
}


case class UguisuEvaluationTablePlayer[D <: Disk](override val disk: D) extends EvaluationTablePlayer(
  disk,
  // (from: http://uguisu.skr.jp/othello/5-1.html)
  Seq(
    Seq( 30, -12,  0,  -1,  -1,  0, -12,  30),
    Seq(-12, -15, -3,  -3,  -3, -3, -15, -12),
    Seq(  0,  -3,  0,  -1,  -1,  0,  -3,   0),
    Seq( -1,  -3, -1,  -1,  -1, -1,  -3,  -1),
    Seq( -1,  -3, -1,  -1,  -1, -1,  -3,  -1),
    Seq(  0,  -3,  0,  -1,  -1,  0,  -3,   0),
    Seq(-12, -15, -3,  -3,  -3, -3, -15, -12),
    Seq( 30, -12,  0,  -1,  -1,  0, -12,  30)
  )
)