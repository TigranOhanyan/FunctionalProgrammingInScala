package algorithms.part1.chapter2

import algorithms.util.syntax.accessSyntax

import scala.reflect.ClassTag

object QuickSort {

  def sort[A:ClassTag](arr: Array[A])(predicate: (A, A) => Boolean): Array[A] = {
    val arrCopy = Array(arr: _*)
    quicksort(arrCopy, 1, arrCopy.length)(predicate)
    arrCopy
  }

  private def partition[A](arr: Array[A], p: Int, r: Int)(predicate: (A, A) => Boolean): Int = {
    val pivot = arr.access(r)
    var i = p - 1
    for (j <- p until r) {
      val currentElem = arr.access(j)
      if (predicate(currentElem, pivot)) {
        i += 1
        arr.exchange(i, j)
      }
    }
    i += 1
    arr.exchange(i, r)
    i
  }

  private def quicksort[A](arr: Array[A], p: Int, r: Int)(predicate: (A, A) => Boolean): Unit = {
    if (p < r) {
      val q = partition(arr, p, r)(predicate)
      quicksort(arr, p, q - 1)(predicate)
      quicksort(arr, q + 1, r)(predicate)
    }
  }

}
