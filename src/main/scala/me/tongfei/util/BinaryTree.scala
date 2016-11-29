package me.tongfei.util

import scala.collection._

/**
 * @author Tongfei Chen
 */
case class BinaryTree[T](data: T, left: BinaryTree[T], right: BinaryTree[T], var parent: BinaryTree[T]) { self =>

  def preOrder = new Iterable[BinaryTree[T]] {
    def iterator = new Iterator[BinaryTree[T]] {
      private[this] val stack = new mutable.ArrayStack[BinaryTree[T]]()
      stack.push(self)

      def hasNext = stack.nonEmpty

      def next() = {
        val curr = stack.pop()
        if (curr.right != null) stack.push(curr.right)
        if (curr.left != null) stack.push(curr.left)
        curr
      }
    }
  }

}
