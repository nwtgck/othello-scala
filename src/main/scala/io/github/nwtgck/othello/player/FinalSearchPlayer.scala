package io.github.nwtgck.othello.player

import io.github.nwtgck.othello._

case class FinalSearchPlayer[D <: Disk](override val disk: D, nonFinalPlayer: Player[D]) extends Player[D](disk){
  override def move(board: Board): Position = {
    // Count empty cell
    val emptyCount = board.countCell(Empty)
    if (emptyCount <= 10) { // TODO: Hard code
      // Perfect search
      MinimaxPlayer(disk, emptyCount, BoardEvaluators.diskDiff).move(board)
    } else if (emptyCount <= 13) { // TODO: Hard code
      // Victory search
      MinimaxPlayer(disk, emptyCount, BoardEvaluators.diskTernaryDiff).move(board)
    } else {
      // Move by non-final player
      nonFinalPlayer.move(board)
    }
  }
}
