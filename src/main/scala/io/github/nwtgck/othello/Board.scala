package io.github.nwtgck.othello

/**
  * Cell
  */
sealed trait Cell

/**
  * Disk (Disk is a subset of Cell)
  */
sealed trait Disk extends Cell{
  /**
    * Reversed disk
    */
  val reversed: Disk = this match {
    case Black => White
    case White => Black
  }
}
case object Black extends Disk
case object White extends Disk
case object Empty extends Cell

case class Position(i: Int, j: Int)

/**
  * Othello Board (Immutable)
  * @param inner
  */
case class Board private (inner: Map[Position, Cell]) {
  import Board.Size

  override def toString: String = {
    val aToH = "  A B C D E F G H\n"

    // Conversion function of Cell to Stirng
    val cellToString: Cell => String = {
      case Black => "*"
      case White => "O"
      case Empty => "-"
    }

    aToH  +
    (0 until Size).map{i =>
      s"${i+1} ${(0 until Size).map{j => cellToString(inner(Position(i, j)))}.mkString(" ")} ${i+1}"
    }.mkString("\n") + "\n" +
    aToH
  }


  // TODO: Make it declarative but it doesn't have side-effects
  private[this] def flipCount(pos: Position, disk: Disk, iAdd: Int, jAdd: Int): Int = {
    import scala.util.control.Breaks

    var Position(i, j) = pos
    if(inner(pos) == Empty) {
      var count = -1
      val b = new Breaks
      b.breakable{
        while(true) {
          count += 1
          i += iAdd
          j += jAdd
          if(!(0 <= i && i < 8 && 0 <= j && j < 8)) {
            return 0
          }
          if(inner(Position(i, j)) != disk.reversed) {
            b.break
          }
        }
      }
      if(count >= 1 && inner(Position(i,j)) == disk) {
        count
      } else {
        0
      }
    } else {
      0
    }
  }

  /**
    * Whether can move or not
    * @param disk
    * @param pos
    * @return
    */
  def canMove(disk: Disk, pos: Position): Boolean =
    flipCount(pos, disk, 0, -1) > 0 ||
    flipCount(pos, disk, 0, 1) > 0 ||
    flipCount(pos, disk, -1, 0) > 0 ||
    flipCount(pos, disk, 1, 0) > 0 ||
    flipCount(pos, disk, -1, -1) > 0 ||
    flipCount(pos, disk, 1, -1) > 0 ||
    flipCount(pos, disk, -1, 1) > 0 ||
    flipCount(pos, disk, 1, 1) > 0

  /**
    * Board after moving a disk
    * @param disk
    * @param pos
    * @return
    */
  // TODO: Make it declarative but it doesn't have side-effects
  def moved(disk: Disk, pos: Position): Board = {
    if(canMove(disk, pos)) {
      var newInner: Map[Position, Cell] = inner
      for {
        dir0 <- -1 to 1
        dir1 <- -1 to 1
        if !(dir0 == 0 && dir1 == 0)
      } {
        var fCount = flipCount(pos, disk, dir0, dir1)
        var Position(newI, newJ) = pos
        while(fCount > 0) {
          newI += dir0
          newJ += dir1
          newInner = newInner.updated(Position(newI,newJ), disk)
          fCount -= 1
        }
      }
      Board(newInner.updated(pos, disk))
    } else {
      this
    }
  }

  /**
    * List movable positions
    * @param disk
    * @return
    */
  def movablePositions(disk: Disk): Seq[Position] =
    for {
      i <- 0 until Size
      j <- 0 until Size
      pos = Position(i, j)
      if this.canMove(disk, pos)
    } yield pos
}


object Board {

  /**
    * Width and height of board
    */
  val Size = 8

  /**
    * Initial board
    */
  val initial: Board =
    Board(Map.empty.withDefault{
      case Position(3, 3) => White
      case Position(3, 4) => Black
      case Position(4, 3) => Black
      case Position(4, 4) => White
      case _              => Empty
    })
}
