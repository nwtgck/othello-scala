package io.github.nwtgck.othello

/**
  * Position of othello game
  * @param board Board
  * @param disk current disk
  * @param passedInPrevious has been passed before game position
  */
case class GamePosition(board: Board, disk: Disk, passedInPrevious: Boolean) {
  /**
    * Movable positions of current game position
    */
  val movablePositions: Seq[Position] = board.movablePositions(disk)
}
