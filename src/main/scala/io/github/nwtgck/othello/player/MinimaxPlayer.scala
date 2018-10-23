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
        ))
      )
    }
  }


  private def minimax(depthLimit: Int, gameTree: GameTree): Int = {
    if(depthLimit == 0 || gameTree.children.isEmpty) {
      evaluateGamePosition(gameTree.value) * (if(gameTree.value.disk == disk) 1 else -1)
    } else {
      val evaluatedValues: Seq[Int] = for {
        childTree <- gameTree.children
      } yield {
        minimax(depthLimit - 1, childTree)
      }
      if(gameTree.value.disk == disk) {
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
