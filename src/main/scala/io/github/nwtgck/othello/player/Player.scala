package io.github.nwtgck.othello.player

import io.github.nwtgck.othello.{Board, Disk}

abstract class Player[+D <: Disk] (val disk: D) {
  def move(board: Board): Board.Position
}
