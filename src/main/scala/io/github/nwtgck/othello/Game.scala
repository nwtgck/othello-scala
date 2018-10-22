package io.github.nwtgck.othello

import io.github.nwtgck.othello.player.Player

/**
  * Othello game (mutable)
  */
class Game(player1: Player[Black.type], player2: Player[White.type]) {
  def start(): Unit = {
    // Set initial board
    var board: Board = Board.initial
    // Pass count
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
        // Increment pass count
        passCount += 1
        // Switch the player
        switchPlayer()
      } else {
        // Decide the position to move until the position is movable
        val pos: Board.Position = Iterator.continually(
          currPlayer.move(board)
        ).find(pos =>
          board.canMove(currPlayer.disk, pos)
        ).head

        // Update the board to moved board
        board = board.moved(currPlayer.disk, pos)

        // Switch the player
        switchPlayer()
      }
    }

  }
}
