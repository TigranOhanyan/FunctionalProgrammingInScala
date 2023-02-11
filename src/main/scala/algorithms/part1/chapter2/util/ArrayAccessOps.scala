package algorithms.part1.chapter2.util

class ArrayAccessOps[T](val arr: Array[T]) extends AnyVal {

  def access(i: Int): T = arr(i - 1)
  def exchange(i: Int, j: Int): Unit = {
    val iElem = access(i)
    val jElem = access(j)
    arr(i - 1) = jElem
    arr(j - 1) = iElem
  }

  def replace(i: Int, elem: T): Unit = arr(i - 1) = elem

  def cut(start: Int, until: Int): Array[T] = arr.slice(start - 1, until - 1)

}

trait ArrayAccessSyntax {
  implicit def accessSyntax[T](arr: Array[T]): ArrayAccessOps[T] = new ArrayAccessOps[T](arr)
}
