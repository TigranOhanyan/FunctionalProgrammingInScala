package part1.chapter2

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.reflect.ClassTag

object ArraySorting {

  object Mutable {

    def inSorted[A](as: Array[A], ordered: (A, A) => Boolean): Unit = {

      def sort(start: Int, end: Int): Unit = {
        if (start < end) {
          val mid = (start + end) / 2
          sort(start, mid)
          sort(mid + 1, end)
          merge(start, mid, end)
        }
      }

      def merge(start: Int, mid: Int, end: Int): Unit = {
        val L: Array[A] = as.slice(start, mid + 1)
        val R: Array[A] = as.slice(mid + 1, end + 1)

        var i = 0
        var j = 0

        for (k <- start to end) {
          if (i < L.length && j < R.length) {
            if (ordered(L(i), R(j))) {
              as(k) = L(i)
              i += 1
            } else {
              as(k) = R(j)
              j += 1
            }
          } else if (i < L.length) {
            as(k) = L(i)
            i += 1
          } else if (j < R.length) {
            as(k) = R(j)
            j += 1
          }
        }
      }

      sort(0, as.length - 1)
    }
  }

  object Immutable {

    sealed trait Path[A]

    object Path {

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

//      def tryGoToRight[A](start: Int, end: Int): Path[A] = {
//        val mid = (start - end) / 2
//        if (start < mid)
//          ToRight[A](start, mid)
//        else
//          Destination[A](mid)
//      }
    }

    case class ToLeft[A](start: Int, end: Int) extends Path[A]

    case class ToRight[A](leftResult: Array[A], start: Int, end: Int)
        extends Path[A]

    case class Destination[A](index: Int) extends Path[A]

    def inSorted[A:ClassTag](as: Array[A], ordered: (A, A) => Boolean): Array[A] = {

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
      def bubble(sorted: Array[A], path: List[Path[A]]): (List[Path[A]], Array[A]) =
        path match {
          case Nil                              => (path, sorted)
          case ToRight(leftResult, _, _) :: tail =>
            val mergedArray = merge(sorted, leftResult)
            bubble(mergedArray, tail)
          case ToLeft(start, end) :: tail       => (ToRight(sorted, start, end) :: tail, sorted)
          case _ :: tail                        => bubble(sorted, tail)
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
  }
}
