package io.github.nwtgck.othello

import java.io.PrintStream

import io.github.nwtgck.othello.player._

import scala.io.StdIn
import scala.util.Try

object Main {

  def main(args: Array[String]): Unit = {
    // Create players
    var player1 = selectDelayedPlayer("Player1", Black)()
    var player2 = selectDelayedPlayer("Player2", White)()

    // The number of games
    val nGames = selectNGames("")

    if(nGames == 1) {
      new Game(player1, player2).start(logPrintStream = System.out)
    } else {
      val finalBoards = for(n <- 1 to nGames) yield {
        // Start the game and get final result
        val finalBoard = new Game(player1, player2).start(logPrintStream = new PrintStream((b: Int) => ()))
        println(s"#${n} Black: ${finalBoard.blackCount}, White: ${finalBoard.whiteCount}")
        finalBoard
      }

      val blackWinningRate = finalBoards.count(b => b.blackCount > b.whiteCount).toFloat / nGames
      val whiteWinningRate = finalBoards.count(b => b.whiteCount > b.blackCount).toFloat / nGames
      val drawRate         = finalBoards.count(b => b.blackCount == b.whiteCount).toFloat / nGames
      println(s"Black winning rate: ${blackWinningRate}, White winning rate: ${whiteWinningRate}, draw rate: ${drawRate}")
    }
  }

  private def selectValue[T](prefix: String, converter: String => T): T = {
    Iterator.continually(
      Try(converter(StdIn.readLine(s"${prefix}> "))).toOption
    ).find { value =>
      value.isDefined
    }.head.get // NOTE: Logically safe .head.get because not empty sequence and defined
  }

  private def selectList[T](prefix: String, itemAndCandidates: Seq[(String, T)]): T = {
    val items: Seq[String] = itemAndCandidates.map(_._1)
    val num: Int = Iterator.continually(
      Try(StdIn.readLine(s"${prefix}| ${items.zipWithIndex.map{case (item, i) => s"${i+1}: ${item}"}.mkString(" ")}> ").toInt).toOption
    ).find { numOpt =>
      numOpt.isDefined && itemAndCandidates.isDefinedAt(numOpt.get-1)
    }.head.get // NOTE: Logically safe .head.get because not empty sequence and defined

    itemAndCandidates(num -1)._2
  }

  private def selectNGames(prefix: String): Int =
    selectValue[Int](s"${prefix}/# of games", _.toInt)

  private def selectDepthLimit(prefix: String): Int =
    selectValue[Int](s"${prefix}/depth limit", _.toInt)

  private def selectBoardEvaluator(prefix: String): (Board, Disk) => Int =
    selectList(s"${prefix}/board evaluators", Seq(
      ("uguisu", BoardEvaluators.uguisi)
    ))

  private def selectRandomSeed(prefix: String): Long =
    selectValue(s"${prefix}/seed", _.toLong)

  private def selectNTrials(prefix: String): Int =
    selectValue(s"${prefix}/# of trials", _.toInt)

  private def selectDelayedPlayer[D <: Disk](prefix: String, disk: D): () => Player[D] =
    selectList(s"${prefix}", Seq(
      ("human", () => HumanPlayer(disk)),
      ("random", () => RandomPlayer(disk, selectRandomSeed(s"${prefix}/random"))),
      ("Monte Carlo", () => MonteCarloPlayer(disk, selectRandomSeed(s"${prefix}/Monte Carlo"), selectNTrials(s"${prefix}/Monte Carlo"))),
      ("Uguisu table", () => UguisuEvaluationTablePlayer(disk)),
      ("minimax", () => MinimaxPlayer(disk, selectDepthLimit(s"${prefix}/minimax"), selectBoardEvaluator(s"${prefix}/minimax"))),
      ("final-search", () => FinalSearchPlayer(disk, selectDelayedPlayer(s"${prefix}/final-search", disk)()))
    ))
}
