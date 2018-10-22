package io.github.nwtgck.othello

import io.github.nwtgck.othello.player._

import scala.io.StdIn
import scala.util.Try

object Main {


  private def selectRandomSeed(): Long =
    Iterator.continually(
      Try(StdIn.readLine("seed> ").toLong).toOption
    ).find { seedOpt =>
      seedOpt.isDefined
    }.head.get // NOTE: Logically safe .head.get because not empty sequence and defined

  private def selectNTrials(): Int =
    Iterator.continually(
      Try(StdIn.readLine("# of trials> ").toInt).toOption
    ).find { seedOpt =>
      seedOpt.isDefined
    }.head.get // NOTE: Logically safe .head.get because not empty sequence and defined

  private def selectPlayer[D <: Disk](disk: D): Player[D] = {
    // Candidate players
    val delayedPlayers: Seq[() => Player[D]] =
      Seq(
        () => HumanPlayer(disk),
        () => RandomPlayer(disk, selectRandomSeed()),
        () => MonteCarloPlayer(disk, selectRandomSeed(), selectNTrials()),
        () => UguisuEvaluationTablePlayer(disk)
      )

    val playerIdx: Int = Iterator.continually(
      Try(StdIn.readLine("0: human, 1: random, 2: Monte Carlo, 3: Uguisu table> ").toInt).toOption
    ).find { idxOpt =>
      idxOpt.isDefined && delayedPlayers.isDefinedAt(idxOpt.get)
    }.head.get // NOTE: Logically safe .head.get because not empty sequence and defined

    delayedPlayers(playerIdx)()
  }

  def main(args: Array[String]): Unit = {

    // Create players
    var player1 = selectPlayer(Black)
    var player2 = selectPlayer(White)

    // Start the game
    new Game(player1, player2).start()
  }
}
