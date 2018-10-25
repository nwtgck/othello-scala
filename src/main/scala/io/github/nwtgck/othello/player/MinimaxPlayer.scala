package io.github.nwtgck.othello.player

import io.github.nwtgck.othello._
import io.github.nwtgck.tree.LazyTree

case class MinimaxPlayer[D <: Disk](override val disk: D, depthLimit: Int, boardEvaluator: (Board, Disk) => Int) extends Player[D](disk){
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
      boardEvaluator(gameTree.value.board, disk)
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
}
