package io.github.nwtgck.othello

import io.github.nwtgck.othello.player.Player

/**
  * Othello game (mutable)
  */
class Game(player1: Player[Black.type], player2: Player[White.type]) {

  private def positionToMoveStr(pos: Position): String = {
    val Position(i, j) = pos
    s"${(j + 'a'.toInt).toChar}${i+1}"
  }

  def start(): Unit = {
    // Set initial board
    var board: Board = Board.initial
    // Continuous pass count
    var passCount: Int = 0
    // Current player
    var currPlayer: Player[Disk] = player1

    def switchPlayer(): Unit ={
      currPlayer = if (currPlayer eq player1) player2 else player1
    }

    while(passCount != 2) {
      // Print current board
      println(board)
      val movablePoss = board.movablePositions(currPlayer.disk)
      // If the player should pass
      if(movablePoss.isEmpty) {
        // Print pass
        println(s"${currPlayer.disk} pass!")
        // Increment pass count
        passCount += 1
        // Switch the player
        switchPlayer()
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

        // Print move
        println(s"${currPlayer.disk} moved at ${positionToMoveStr(pos)}!")

        // Switch the player
        switchPlayer()
      }
    }

    // Print result
    println(s"Black: ${board.blackCount}, White: ${board.whiteCount}")
  }
}
