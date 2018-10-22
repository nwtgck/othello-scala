package io.github.nwtgck.othello

object Utils {
  def positionToMoveStr(pos: Position): String = {
    val Position(i, j) = pos
    s"${(j + 'a'.toInt).toChar}${i + 1}"
  }
}
