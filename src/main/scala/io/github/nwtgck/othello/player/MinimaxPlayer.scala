package io.github.nwtgck.othello.player

import io.github.nwtgck.othello._
import io.github.nwtgck.tree.LazyTree

case class MinimaxPlayer[D <: Disk](override val disk: D, depthLimit: Int) extends Player[D](disk){
  type GameTree = LazyTree[GamePosition]

  override def move(board: Board): Position = {
    board.movablePositions(disk).maxBy{pos =>
      minimax(
        depthLimit = depthLimit,
        gameTree = GameTree.create(GamePosition(
          board = board.moved(disk, pos),
          disk = disk.reversed,
          previousMoveOpt = Some(Move.At(pos))
        )),
        _alpha = Int.MinValue,
        _beta  = Int.MaxValue
      )
    }
  }

  private def minimax(depthLimit: Int, gameTree: GameTree, _alpha: Int, _beta: Int): Int = {
    val isMyTurn: Boolean = gameTree.value.disk == disk

    if(depthLimit == 0 || gameTree.children.isEmpty) {
      evaluateGamePosition(gameTree.value) * (if(isMyTurn) 1 else -1)
    } else {
      var alpha = _alpha
      var beta  = _beta

      val evaluatedValues: Seq[Int] = for {
        childTree <- gameTree.children
      } yield {
        val score = minimax(depthLimit - 1, childTree, alpha, beta)
        if(isMyTurn) {
          if(score >= beta) {
            return score
          }
          alpha = Math.max(alpha, score)
        } else {
          if(score <= alpha) {
            return score
          }
          beta = Math.min(beta, score)
        }
        score
      }
      if(isMyTurn) {
        evaluatedValues.max
      } else {
        evaluatedValues.min
      }
    }
  }

  private def evaluateGamePosition(gamePosition: GamePosition): Int = {
    // TODO: Hard code
    // TODO: Duplicate code
    // (from: http://uguisu.skr.jp/othello/5-1.html)
    val table = Seq(
      Seq( 30, -12,  0,  -1,  -1,  0, -12,  30),
      Seq(-12, -15, -3,  -3,  -3, -3, -15, -12),
      Seq(  0,  -3,  0,  -1,  -1,  0,  -3,   0),
      Seq( -1,  -3, -1,  -1,  -1, -1,  -3,  -1),
      Seq( -1,  -3, -1,  -1,  -1, -1,  -3,  -1),
      Seq(  0,  -3,  0,  -1,  -1,  0,  -3,   0),
      Seq(-12, -15, -3,  -3,  -3, -3, -15, -12),
      Seq( 30, -12,  0,  -1,  -1,  0, -12,  30)
    )
    // Position to Evaluation
    val posToEvaluation: Map[Position, Int] =
      (for {
        i <- 0 until Board.Size
        j <- 0 until Board.Size
      } yield (Position(i, j), table(i)(j))).toMap

    // Sum all evaluation values
    (for {
      i <- 0 until Board.Size
      j <- 0 until Board.Size
    } yield {
      val pos = Position(i, j)
      val cell = gamePosition.board(pos)
      posToEvaluation(pos) * (cell match {
        case Empty => 0
        case gamePosition.disk => 1
        case _ => -1
      })
    }).sum
  }
}
