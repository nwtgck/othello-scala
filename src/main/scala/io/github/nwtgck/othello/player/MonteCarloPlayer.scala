package io.github.nwtgck.othello.player

import io.github.nwtgck.othello.{Board, Disk, Position, Utils}

import scala.util.Random

case class MonteCarloPlayer[D <: Disk](override val disk: D, randomSeed: Long, nTrials: Int) extends Player[D](disk) {
  private[this] def evaluate(_board: Board, seed: Long): Int = {
    val player1 = RandomPlayer(disk.reversed, seed)
    val player2 = RandomPlayer(disk, seed)
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
    val random = new Random(randomSeed)
    val movablePoss = board.movablePositions(disk)
    val posAndScores: Seq[(Position, Int)] = movablePoss.map(pos =>
      (
        pos,
        (1 to nTrials)
          .map(_ => random.nextLong)
          .par
          .map(seed => evaluate(board.moved(disk, pos), seed))
          .sum
      )
    )

    // Print position and its score
    println(
      posAndScores
        .sortBy{case (_, score) => -score}
        .map{case (pos, score) =>
          s"${Utils.positionToMoveStr(pos)}: ${score}"
        }
        .mkString(", ")
    )

    posAndScores.maxBy{case (_, score) => score}._1
  }
}
