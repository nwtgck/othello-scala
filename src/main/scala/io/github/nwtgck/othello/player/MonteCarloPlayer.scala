package io.github.nwtgck.othello.player

import io.github.nwtgck.othello.{Board, Disk, Position}

import scala.util.Random

case class MonteCarloPlayer[D <: Disk](override val disk: D, randomSeed: Long) extends Player[D](disk) {

  val random = new Random(randomSeed)

  private[this] def evaluate(_board: Board): Int = {

    val player1 = RandomPlayer(disk.reversed, random.nextLong)
    val player2 = RandomPlayer(disk, random.nextLong)
    var board = _board

    // Continuous pass count
    var passCount: Int = 0
    // Current player
    var currPlayer: Player[Disk] = player1

    while(passCount != 2) {
      val movablePoss = board.movablePositions(currPlayer.disk)
      // If the player should pass
      if(movablePoss.isEmpty) {
        // Increment pass count
        passCount += 1
      } else {
        // Reset continuous pass count
        passCount = 0
        // Decide the position to move until the position is movable
        val pos: Position = Iterator.continually(
          currPlayer.move(board)
        ).find(pos =>
          board.canMove(currPlayer.disk, pos)
        ).head

        // Update the board to moved board
        board = board.moved(currPlayer.disk, pos)
      }

      // Switch the player
      currPlayer = if (currPlayer eq player1) player2 else player1
    }

    val myCount: Int = board.countCell(disk)
    val yourCount: Int = board.countCell(disk.reversed)
    if (myCount == yourCount) {
      0
    } else if(myCount < yourCount) {
      -1
    } else {
      1
    }
  }

  override def move(board: Board): Position = {
    val nTrials = 100
    val movablePoss = board.movablePositions(disk)
    movablePoss.par.maxBy(pos =>
      (1 to nTrials).map(_ => evaluate(board.moved(disk, pos))).sum.toFloat / nTrials
    )
  }
}
