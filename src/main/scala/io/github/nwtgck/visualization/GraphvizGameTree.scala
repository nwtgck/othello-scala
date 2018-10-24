package io.github.nwtgck.visualization

import io.github.nwtgck.othello._
import io.github.nwtgck.tree.LazyTree

import scala.collection.immutable.Queue

object GraphvizGameTree {
  private def createDotLines(linesAfterNodeDef: Seq[String], nodeId: String, depthLimit: Int, gameTree: LazyTree[GamePosition]): Seq[String] = {
    if(depthLimit == 0) {
      Queue.empty
    } else {
      val gamePosition = gameTree.value
      Queue[String](
        s""""${nodeId}" [label=<${boardToHtmlTable(gamePosition.board)}>]"""
      ) ++
      linesAfterNodeDef ++
      Queue(gameTree.children.zipWithIndex: _*).flatMap{ case (childTree, i) =>
        val childNodeId = s"${nodeId}-${i}"
        val headLabel = childTree.value.previousMoveOpt match {
          case Some(Move.At(pos)) => Utils.positionToMoveStr(pos)
          case Some(Move.Pass)    => "pass"
          case None               => ""
        }
        createDotLines(
          Queue(s""""${nodeId}" -> "${childNodeId}" [headlabel="${headLabel}"]"""),
          childNodeId,
          depthLimit-1,
          childTree
        )
      }
    }
  }

  // Create HTML table string
  private def boardToHtmlTable(board: Board): String = {
    "<table border='1' color='#404040' cellspacing='0'>" +
    (for(i <- 0 until Board.Size) yield {
      "<tr>" +
      (for(j <- 0 until Board.Size) yield {
        val cell = board(Position(i, j))
        val bgColor = cell match {
          case Empty => "#00b33c"
          case Black => "black"
          case White => "white"
        }
        s"<td bgcolor='${bgColor}'></td>"
      }).mkString +
      "</tr>"
    }).mkString +
    "</table>"
  }

  /**
    * Create .dot tree string
    * @param depthLimit
    * @param board
    * @param disk
    * @return
    */
  def dotTreeString(depthLimit: Int, board: Board, disk: Disk): String = {
    val indent = "  "
    val dotLines = createDotLines(Queue.empty, "0", depthLimit, GameTree.create(GamePosition(board, disk, None)))
    s"""
      |digraph Tree {
      |${indent}edge [labeldistance=2,labelfloat=true]
      |${indent}node [shape=box] ;
      |${dotLines.map(indent + _).mkString("\n")}
      |}
    """.stripMargin
  }
}
