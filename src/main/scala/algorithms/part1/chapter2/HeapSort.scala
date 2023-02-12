package algorithms.part1.chapter2

import algorithms.util.syntax._

import scala.annotation.tailrec

object HeapSort {

  def parent(i: Int): Int = i / 2

  def left(i: Int): Int = 2 * i

  def right(i: Int): Int = 2 * i + 1

  def heapify[T](arr: Array[T], i: Int, heapSize: Int)(predicate: (T, T) => Boolean): Unit = {
   @tailrec
    def loop(i: Int): Unit = {
      val l = left(i)
      val r = right(i)
      var largest = i
      if (l <= heapSize && !predicate(arr.access(l), arr.access(largest))) {
        largest = l
      }
      if (r <= heapSize && !predicate(arr.access(r), arr.access(largest))) {
        largest = r
      }
      if (largest != i) {
        arr.exchange(i, largest)
        loop(largest)
      } else ()
    }
    loop(i)
  }

  def buildHeap[T](arr: Array[T])(predicate: (T, T) => Boolean): Unit = {
    val heapSize = arr.length
    val startingIndex = arr.length / 2

    @tailrec def loop(i: Int): Unit =
      if (i >= 1) {
        heapify(arr, i, heapSize)(predicate)
        loop(i - 1)
      } else ()

    loop(startingIndex)
  }

  def sort[T](arr: Array[T])(predicate: (T, T) => Boolean): Unit = {
    buildHeap(arr)(predicate)

    @tailrec def loop(i: Int, heapSize: Int): Unit = {
      if (i >= 2) {
        arr.exchange(1, i)
        heapify(arr, 1, heapSize - 1)(predicate)
        loop(i - 1, heapSize - 1)
      }
    }

    loop(arr.length, arr.length)
  }
}
