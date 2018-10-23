package io.github.nwtgck.othello

/**
  * Move or pass
  */
sealed trait Move
object Move {
  case object Pass extends Move
  case class At(position: Position) extends Move
}

/**
  * Position of othello game
  * @param board Board
  * @param disk current disk
  * @param previousMoveOpt Previous move (None: initial game should not have previous move)
  */
case class GamePosition(board: Board, disk: Disk, previousMoveOpt: Option[Move]) {
  /**
    * Movable positions of current game position
    */
  val movablePositions: Seq[Position] = board.movablePositions(disk)
}
