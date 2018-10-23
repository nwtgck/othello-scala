package io.github.nwtgck.othello

import io.github.nwtgck.othello.player.{MonteCarloPlayer, RandomPlayer}

object BenchmarkMain {

  def simpleBenchmark(title: String)(f: => Unit): Unit = {
    println(s"====== ${title} ======")
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    println(s"passed: ${(end - start).toFloat / 1000} sec")
  }

  def main(args: Array[String]): Unit = {

    // Create players
    var player1 = RandomPlayer(Black, 1)
    var player2 = MonteCarloPlayer(White, 1, 100)

    simpleBenchmark("") {
      // Start the game
      new Game(player1, player2).start()
    }
  }
}
