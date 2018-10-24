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
        s""""${nodeId}" [label=${Utils.escapedString(gamePosition.board.toString)}]"""
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
