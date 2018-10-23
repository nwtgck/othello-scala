package io.github.nwtgck.othello

import io.github.nwtgck.tree.LazyTree

object GameTree {

  /**
    * Create game tree starting from the specified game position
    * @param gamePosition
    * @return
    */
  def create(gamePosition: GamePosition): LazyTree[GamePosition] =
    LazyTree.node(gamePosition, createChildTrees(gamePosition))

  private def createChildTrees(gamePosition: GamePosition): Stream[LazyTree[GamePosition]] = {
    // Get all movable positions
    val movablePoss = gamePosition.movablePositions
    // If the player should pass
    if(movablePoss.isEmpty) {
      gamePosition.previousMoveOpt match {
        // If has been passed in previous position
        case Some(previousMove) if previousMove == Move.Pass =>
          // Finish game
          Stream.empty
        case _ =>
          // Get next game position
          val nextGamePosition = gamePosition.copy(
            disk = gamePosition.disk.reversed,
            previousMoveOpt = Some(Move.Pass)
          )
          // Create node and create children tree recursively
          Stream(LazyTree.node(nextGamePosition, createChildTrees(nextGamePosition)))
      }
    } else {
      movablePoss.toStream.map{pos =>
        // Get next board after moving
        val nextBoard = gamePosition.board.moved(gamePosition.disk, pos)
        // Get next game position
        val nextGamePosition = GamePosition(
          board = nextBoard,
          disk = gamePosition.disk.reversed,
          previousMoveOpt = Some(Move.At(pos))
        )
        // Create node and create children tree recursively
        LazyTree.node(nextGamePosition, createChildTrees(nextGamePosition))
      }
    }
  }
}
