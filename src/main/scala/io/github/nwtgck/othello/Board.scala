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

/**
  * Othello Board (Immutable)
  * @param inner
  */
case class Board private (inner: Map[Board.Position, Cell]) {
  import Board.{Position, Size}

  override def toString: String = {
    val aToH = "  A B C D E F G H\n"

    // Conversion function of Cell to Stirng
    val cellToString: Cell => String = {
      case Black => "*"
      case White => "O"
      case Empty => "-"
    }

    aToH  +
    (0 until Size).map{h =>
      s"${h+1} ${(0 until Size).map{w => cellToString(inner((h, w)))}.mkString(" ")} ${h+1}"
    }.mkString("\n") + "\n" +
    aToH
  }


  // TODO: Make it declarative but it doesn't have side-effects
  private[this] def flipCount(pos: Position, disk: Disk, hAdd: Int, wAdd: Int): Int = {
    import scala.util.control.Breaks

    var (h: Int, w: Int) = pos
    if(inner(pos) == Empty) {
      var count = -1
      val b = new Breaks
      b.breakable{
        while(true) {
          count += 1
          h += hAdd
          w += wAdd
          if(!(0 <= h && h < 8 && 0 <= w && w < 8)) {
            return 0
          }
          if(inner((h, w)) != disk.reversed) {
            b.break
          }
        }
      }
      if(count >= 1 && inner((h,w)) == disk) {
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
        var (newH, newW) = pos
        while(fCount > 0) {
          newH += dir0
          newW += dir1
          newInner = newInner.updated((newH,newW), disk)
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
      h <- 0 until Size
      w <- 0 until Size
      if this.canMove(disk, (h, w))
    } yield (h, w)
}


object Board {
  type Position = (Int, Int)

  /**
    * Width and height of board
    */
  val Size = 8

  /**
    * Initial board
    */
  val initial: Board =
    Board(Map.empty.withDefault{
      case (3, 3) => White
      case (3, 4) => Black
      case (4, 3) => Black
      case (4, 4) => White
      case _      => Empty
    })
}
