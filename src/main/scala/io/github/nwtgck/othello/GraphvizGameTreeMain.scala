package io.github.nwtgck.othello

import io.github.nwtgck.visualization.GraphvizGameTree

object GraphvizGameTreeMain {
  def main(args: Array[String]): Unit = {
    println(GraphvizGameTree.dotTreeString(depthLimit = 8, Board.initial, Black))
  }
}
