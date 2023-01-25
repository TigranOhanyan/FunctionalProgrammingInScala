package part1.chapter2

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

//  object Immutable {
//
//    sealed trait Computation[A]
//    case class Sort[A](unsorted: Array[A]) extends Computation[A]
//    case class Carry[A](sortedSibling: Array[A], unsorted: Array[A])
//        extends Computation[A]
//    case class Merge[A](leftSorted: Array[A], rightSorted: Array[A]) extends Computation[A]
//
//    def inSorted[A](as: Array[A], ordered: (A, A) => Boolean): Unit = {
//
//      def mergeSort(computation: List[Computation[A]]): Array[A] =
//        computation match {
//          case computations @ Sort(unsorted) :: tail =>
//            val mid = unsorted.length / 2
//            val left = unsorted.slice(0, mid + 1)
//            lazy val right = unsorted.slice(mid + 1, unsorted.length + 1)
//            if (unsorted.length >= 2) {
//              val computation = Sort[A](left)
//              mergeSort(computation :: computations)
//            } else {
//              val computation = Carry[A](left, right)
//              mergeSort(computation :: tail)
//            }
//          case computations @ Carry(sortedSibling, unsorted) :: tail =>
//            if (unsorted.length > 2) {
//              val computation = Sort[A](unsorted)
//              mergeSort(computation :: computations)
//            } else {
//              val computation = Merge[A](sortedSibling, unsorted)
//              mergeSort(computation :: tail)
//            }
//          case Merge(left, right) :: tail =>
//            var i = 0
//            var j = 0
//            val sorted = new Array[A](left.length + right.length)
//            for (k <- sorted.indices) {
//              if (i < left.length && j < right.length) {
//                if (ordered(left(i), right(j))) {
//                  sorted(k) = left(i)
//                  i += 1
//                } else {
//                  sorted(k) = right(j)
//                  j += 1
//                }
//              } else if (i < left.length) {
//                sorted(k) = left(i)
//                i += 1
//              } else if (j < right.length) {
//                sorted(k) = right(j)
//                j += 1
//              }
//            }
//            tail match {
//              case Sort(unsorted) :: tail      => mergeSort()
//              case Carry(left, mid, end) :: tail =>
//              case Merge(left, right) :: tail        =>
//              case Nil                               =>
//            }
//          case Nil => as
//        }
//    }
//  }
}
