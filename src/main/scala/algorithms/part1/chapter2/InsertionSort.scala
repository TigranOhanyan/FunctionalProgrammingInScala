package algorithms.part1.chapter2
import algorithms.util.syntax._

object InsertionSort {

  object Mutable {

    def sort[T](arr: Array[T])(com: (T, T) => Boolean): Array[T] = {
      for (j <- 2 to arr.length) {
        val key = arr.access(j)
        var i = j - 1
        while (i > 0 && !com(arr.access(i), key)) {
          arr.replace(i + 1, arr.access(i))
          i = i - 1
        }
        arr.replace(i + 1, key)
      }
      arr
    }
  }

}
