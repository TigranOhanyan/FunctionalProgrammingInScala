package algorithms.part1.chapter2

import algorithms.util.syntax._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object MergeSort {

  object Mutable {

    def sort[A](as: Array[A], ordered: (A, A) => Boolean): Unit = {

      def mergesort(start: Int, end: Int): Unit = {
        if (start < end) {
          val mid = (start + end) / 2
          mergesort(start, mid)
          mergesort(mid + 1, end)
          merge(start, mid, end)
        }
      }

      def merge(start: Int, mid: Int, end: Int): Unit = {
        val L: Array[A] = as.cut(start, mid + 1)
        val R: Array[A] = as.cut(mid + 1, end + 1)

        var i = 1
        var j = 1

        for (k <- start to end) {
          if (i <= L.length && j <= R.length) {
            if (ordered(L.access(i), R.access(j))) {
              as.replace(k, L.access(i))
              i += 1
            } else {
              as.replace(k, R.access(j))
              j += 1
            }
          } else if (i <= L.length) {
            as.replace(k, L.access(i))
            i += 1
          } else if (j <= R.length) {
            as.replace(k, R.access(j))
            j += 1
          }
        }
      }

      mergesort(1, as.length)
    }
  }

  object Immutable {

    def sort[A: ClassTag](
        as: Array[A],
        ordered: (A, A) => Boolean
    ): Array[A] = {

      def merge(left: Array[A], right: Array[A]): Array[A] = {
        val empty = Array.ofDim[A](left.length + right.length)

        var i = 0
        var j = 0

        for (k <- empty.indices) {
          if (i < left.length && j < right.length) {
            if (ordered(left(i), right(j))) {
              empty(k) = left(i)
              i += 1
            } else {
              empty(k) = right(j)
              j += 1
            }
          } else if (i < left.length) {
            empty(k) = left(i)
            i += 1
          } else if (j < right.length) {
            empty(k) = right(j)
            j += 1
          }
        }
        empty
      }

      @tailrec
      def drown(path: List[Path[A]]): List[Path[A]] = path match {
        case ToLeft(start, end) :: _ =>
          drown(Path.tryGoToLeft[A](start, end) :: path)
        case ToRight(_, start, end) :: _ =>
          drown(Path.tryGoToRight[A](start, end) :: path)
        case _ => path
      }

      @tailrec
      def bubble(
          sorted: Array[A],
          path: List[Path[A]]
      ): (List[Path[A]], Array[A]) =
        path match {
          case Nil => (path, sorted)
          case ToRight(leftResult, _, _) :: tail =>
            val mergedArray = merge(sorted, leftResult)
            bubble(mergedArray, tail)
          case ToLeft(start, end) :: tail =>
            (ToRight(sorted, start, end) :: tail, sorted)
          case _ :: tail => bubble(sorted, tail)
        }

      @tailrec
      def rec(sorted: Array[A], path: List[Path[A]]): Array[A] = path match {
        case Nil => sorted
        case Destination(v) :: _ =>
          val arr = Array(as(v))
          val (newSplittingPoint, latestSortedArray) = bubble(arr, path)
          rec(latestSortedArray, newSplittingPoint)
        case path =>
          val p = drown(path)
          rec(sorted, p)
      }

      val start = 0
      val end = as.length - 1

      if (start < end) rec(Array.empty[A], ToLeft[A](start, end) :: Nil)
      else as
    }

    private sealed trait Path[A]

    private object Path {

      def tryGoToLeft[A](start: Int, end: Int): Path[A] = {
        val mid = (end + start) / 2
        if (start < mid)
          ToLeft[A](start, mid)
        else
          Destination[A](mid)
      }

      def tryGoToRight[A](start: Int, end: Int): Path[A] = {
        val mid = (end + start) / 2
        if (mid + 1 < end)
          ToLeft[A](mid + 1, end)
        else
          Destination[A](mid + 1)
      }
    }

    private case class ToLeft[A](start: Int, end: Int) extends Path[A]

    private case class ToRight[A](leftResult: Array[A], start: Int, end: Int)
        extends Path[A]

    private case class Destination[A](index: Int) extends Path[A]

  }
}
