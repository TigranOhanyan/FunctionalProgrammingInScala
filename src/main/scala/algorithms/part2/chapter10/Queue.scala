package algorithms.part2.chapter10

import algorithms.part2.{Overflow, Underflow}
import algorithms.util.syntax._

import scala.reflect.ClassTag

class Queue[T: ClassTag](limit: Int) {

  private val n: Int = limit + 1

  private[chapter10] val array: Array[T] = new Array[T](n)

  private[chapter10] var head: Int = 1

  private[chapter10] var tail: Int = 1

  private def nextIndex(i: Int): Int = if (i < n) i + 1 else 1

  def enqueue(v: T): Unit = {
    if (nextIndex(tail) != head) {
      array.replace(tail, v)
      tail = nextIndex(tail)
    } else throw Overflow
  }

  def dequeue(): T = {
    if (tail != head) {
      val elem = array.access(head)
      head = nextIndex(head)
      elem
    } else throw Underflow
  }

}
