package me.tongfei.util

import poly.io.Local._

/**
 * @author Tongfei Chen
 */
object BisectingKMeans {

  type Vec = Array[Double]

  case class Cluster(items: Seq[(Vec, Int)], var id: String)

  implicit class VecOps(val x: Vec) extends AnyVal {
    def +(y: Vec) = {
      val z = new Vec(x.length)
      for (i <- 0 until x.length) z(i) = x(i) + y(i)
      z
    }

    def /(y: Double) = {
      val z = new Vec(x.length)
      for (i <- 0 until x.length) z(i) = x(i) / y
      z
    }

    def dist(y: Vec) = {
      var sum = 0.0
      for (i <- 0 until x.length) sum += (x(i) - y(i)) * (x(i) - y(i))
      math.sqrt(sum)
    }
  }

  def kMeans(K: Int, xs: Seq[Vec], dist: (Vec, Vec) => Double, maxIteration: Int = 20) = {
    val n = xs.length
    val d = xs.head.length
    val c = Array.fill(2)(new Vec(d))
    val γ = Array.tabulate[Int](n)(_ % K)

    for (iteration <- 0 until maxIteration) {
      // Hard M step
      val count = Array.ofDim[Int](K)
      for (i <- 0 until n) {
        c(γ(i)) += xs(i)
        count(γ(i)) += 1
      }

      for (k <- 0 until K)
        c(k) /= count(k)

      // Hard E step
      for (i <- 0 until n)
        γ(i) = (0 until K) minBy { k => dist(xs(i), c(k)) }
    }

    γ
  }

  def toTree(xs: Seq[(Vec, Int)], minCount: Int): BinaryTree[Cluster] = {
    if (xs.length < minCount)
      BinaryTree(Cluster(xs, ""), null, null, null)
    else {
      val y = kMeans(2, xs.map(_._1), _ dist _)
      val xsL = xs.view.zipWithIndex.filter(p => y(p._2) == 0).map(_._1).force
      val xsR = xs.view.zipWithIndex.filter(p => y(p._2) == 1).map(_._1).force

      if (xsL.length == 0 || xsR.length == 0)
        BinaryTree(Cluster(xs, ""), null, null, null)
      else {
        val l = toTree(xsL, minCount)
        val r = toTree(xsR, minCount)
        println(s"Tree ${xsL.length} ${xsR.length}")
        val node = BinaryTree(Cluster(xs, ""), l, r, null)
        node.left.parent = node
        node.right.parent = node
        node
      }
    }
  }

  def main(args: Array[String]) = {

    val inputFile = args(0)
    val outputFile = args(1)
    val maxItemInCluster = args(2).toInt

    val (xs, words) = File(inputFile).lines.toSeq.map { line =>
      val tokens = line.split("\\s+")
      tokens.drop(1).map(_.toDouble) -> tokens.head
    }.unzip

    val alphabet = words.zipWithIndex.map(_.swap).toMap

    val tree = toTree(xs.zipWithIndex, maxItemInCluster)

    for (node <- tree.preOrder) {
      if (node.parent != null) {
        if (node eq node.parent.left) {
          node.data.id = node.parent.data.id + "0"
        }
        if (node eq node.parent.right)
          node.data.id = node.parent.data.id + "1"
      }
    }

    val pw = new java.io.PrintWriter(outputFile)

    for (node <- tree.preOrder) {
      if (node.left == null && node.right == null) {
        for (item <- node.data.items) {
          pw.println(s"${alphabet(item._2)}\t${node.data.id}")
        }
      }
    }

    pw.close()

  }

}
