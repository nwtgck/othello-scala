package io.github.nwtgck.othello.player
import io.github.nwtgck.othello.{Board, Disk}

import scala.io.StdIn

case class HumanPlayer[D <: Disk](override val disk: D) extends Player[D](disk){

  private def moveStrToPostion(moveStr: String): Option[Board.Position] = {
    moveStr.toList match {
      case List(alphaChar, numChar) =>
        val h = numChar.toInt - '1'.toInt
        val w = Character.toLowerCase(alphaChar).toInt - 'a'
        if(0 <= h && h < Board.Size && 0 <= w && w < Board.Size) {
          Some((h, w))
        } else {
          None
        }
      case _ => None

    }
  }

  override def move(board: Board): (Int, Int) = {
    Iterator.continually(
      moveStrToPostion(StdIn.readLine(s"${disk} Pos (e.g. d3)> "))
    ).find{posOpt =>
      posOpt.isDefined
    }.head // NOTE: This is logically safe head because it should not be empty
     .get  // NOTE: This is logically safe get because the option should be defined
  }
}
