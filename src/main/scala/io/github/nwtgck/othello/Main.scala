package io.github.nwtgck.othello

import io.github.nwtgck.othello.player.HumanPlayer

object Main {
  def main(args: Array[String]): Unit = {

    // Create players
    var player1 = HumanPlayer(Black)
    var player2 = HumanPlayer(White)

    // Start the game
    new Game(player1, player2).start()
  }
}
