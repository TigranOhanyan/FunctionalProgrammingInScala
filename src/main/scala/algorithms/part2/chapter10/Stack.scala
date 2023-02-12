package algorithms.part2.chapter10

import algorithms.part2.{Dictionary, NotSupported, Overflow, Underflow}
import algorithms.util.syntax._

import scala.reflect.ClassTag

class Stack[T:ClassTag](n: Int) {

  private[chapter10] val array: Array[T] = new Array[T](n)

  private[chapter10] var top: Int = 0

  def push(v: T): Unit =
    if (top < n) {
      top += 1
      array.replace(top, v)
    } else throw Overflow

  def pop(): T =
    if (top > 0) {
      val elem = array.access(top)
      top -= 1
      elem
    }
    else throw Underflow
}
