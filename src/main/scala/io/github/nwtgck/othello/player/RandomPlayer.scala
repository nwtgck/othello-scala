package io.github.nwtgck.othello.player

import io.github.nwtgck.othello.{Board, Disk}

import scala.util.Random


case class RandomPlayer[D <: Disk](override val disk: D, random: Random) extends Player[D](disk){
  override def move(board: Board): Board.Position = {
    val pos = board.movablePositions(disk)
    pos(random.nextInt(pos.length))
  }
}
