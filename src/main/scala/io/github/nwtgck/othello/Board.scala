package io.github.nwtgck.othello

sealed trait Color
sealed trait Cell

case object Black extends Color with Cell
case object White extends Color with Cell
case object Empty extends Cell


// TODO: Change it to be immutable
class Board {
  /**
    * Width and height of board
    */
  val Size = 8

  // Set initial board
  private[this] val inner: Array[Array[Cell]] =
    Array.fill(Size)(Array.fill(Size)(Empty))

  // Set initial position
  inner(3)(3) = White
  inner(3)(4) = Black
  inner(4)(3) = Black
  inner(4)(4) = White

  override def toString: String = {
    val aToH = "  A B C D E F G H\n"

    // Conversion function of Cell to Stirng
    val cellToString: Cell => String = {
      case Black => "*"
      case White => "O"
      case Empty => "-"
    }

    aToH  +
    inner.zipWithIndex.map { case (row, i) =>
      s"${i+1} ${row.map(cellToString).mkString(" ")} ${i+1}"
    }.mkString("\n") + "\n" +
    aToH
  }
}
