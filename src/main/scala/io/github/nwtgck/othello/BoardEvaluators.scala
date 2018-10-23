package io.github.nwtgck.othello

object BoardEvaluators {
  def uguisi(board: Board, disk: Disk): Int = {
    // TODO: Duplicate code
    // (from: http://uguisu.skr.jp/othello/5-1.html)
    val table = Seq(
      Seq( 30, -12,  0,  -1,  -1,  0, -12,  30),
      Seq(-12, -15, -3,  -3,  -3, -3, -15, -12),
      Seq(  0,  -3,  0,  -1,  -1,  0,  -3,   0),
      Seq( -1,  -3, -1,  -1,  -1, -1,  -3,  -1),
      Seq( -1,  -3, -1,  -1,  -1, -1,  -3,  -1),
      Seq(  0,  -3,  0,  -1,  -1,  0,  -3,   0),
      Seq(-12, -15, -3,  -3,  -3, -3, -15, -12),
      Seq( 30, -12,  0,  -1,  -1,  0, -12,  30)
    )
    // Position to Evaluation
    val posToEvaluation: Map[Position, Int] =
      (for {
        i <- 0 until Board.Size
        j <- 0 until Board.Size
      } yield (Position(i, j), table(i)(j))).toMap

    // Sum all evaluation values
    (for {
      i <- 0 until Board.Size
      j <- 0 until Board.Size
    } yield {
      val pos = Position(i, j)
      val cell = board(pos)
      posToEvaluation(pos) * (cell match {
        case Empty  => 0
        case `disk` => 1
        case _      => -1
      })
    }).sum
  }
}
