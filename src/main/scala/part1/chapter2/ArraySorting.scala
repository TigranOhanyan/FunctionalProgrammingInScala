package part1.chapter2

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.reflect.ClassTag

object ArraySorting {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec def loop(n: Int, previousElem: A): Boolean =
      if (n >= as.length) true
      else {
        val currentElem = as(n)
        if (ordered(previousElem, currentElem)) loop(n + 1, currentElem)
        else false
      }

    if (as.length > 0) loop(1, as(0))
    else true
  }
}
