package io.github.nwtgck.othello.player
import io.github.nwtgck.othello.{Board, Disk, Position}

import scala.io.StdIn

case class HumanPlayer[D <: Disk](override val disk: D) extends Player[D](disk){

  private def moveStrToPostion(moveStr: String): Option[Position] = {
    moveStr.toList match {
      case List(alphaChar, numChar) =>
        val i = numChar.toInt - '1'.toInt
        val j = Character.toLowerCase(alphaChar).toInt - 'a'
        if(0 <= i && i < Board.Size && 0 <= j && j < Board.Size) {
          Some(Position(i, j))
        } else {
          None
        }
      case _ => None

    }
  }

  override def move(board: Board): Position = {
    Iterator.continually(
      moveStrToPostion(StdIn.readLine(s"${disk} Pos (e.g. d3)> "))
    ).find{posOpt =>
      posOpt.isDefined
    }.head // NOTE: This is logically safe head because it should not be empty
     .get  // NOTE: This is logically safe get because the option should be defined
  }
}
