package io.github.nwtgck.tree

class LazyTree[+T] private (_value: => T, val children: Stream[LazyTree[T]]) {
  lazy val value: T = _value

  lazy val isNode: Boolean = children.nonEmpty

  lazy val isLeaf: Boolean = children.isEmpty
}

object LazyTree {
  def node[T](value: => T, children: Stream[LazyTree[T]]): LazyTree[T] =
    new LazyTree[T](value, children)

  def leaf[T](value: => T): LazyTree[T] =
    node(value, Stream.empty)
}
