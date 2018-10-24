package io.github.nwtgck.othello

object Utils {
  def positionToMoveStr(pos: Position): String = {
    val Position(i, j) = pos
    s"${(j + 'a'.toInt).toChar}${i + 1}"
  }


  /**
    * Get escaped string representation
    * @param raw
    * @return
    */
  def escapedString(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }
}
