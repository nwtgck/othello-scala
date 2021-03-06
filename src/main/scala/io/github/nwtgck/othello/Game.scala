package io.github.nwtgck.othello

import java.io.PrintStream

import io.github.nwtgck.othello.player.Player

/**
  * Othello game (mutable)
  */
class Game(player1: Player[Black.type], player2: Player[White.type]) {

  /**
    * Start game
    * @param logPrintStream
    * @return final board
    */
  def start(logPrintStream: PrintStream): Board = {
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
      logPrintStream.println(board)
      val movablePoss = board.movablePositions(currPlayer.disk)
      // If the player should pass
      if(movablePoss.isEmpty) {
        // Print pass
        logPrintStream.println(s"${currPlayer.disk} pass!")
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
        logPrintStream.println(s"${currPlayer.disk} moved at ${Utils.positionToMoveStr(pos)}!")

        // Switch the player
        switchPlayer()
      }
    }

    // Print result
    logPrintStream.println(s"Black: ${board.blackCount}, White: ${board.whiteCount}")

    // Return final board
    board
  }
}
