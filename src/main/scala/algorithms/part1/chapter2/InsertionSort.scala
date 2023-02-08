package algorithms.part1.chapter2

object InsertionSort {

  object Mutable {

    def sort[T](arr: Array[T])(com: (T, T) => Boolean): Array[T] = {
      for (j <- 2 to arr.length) {
        val key = arr(j - 1)
        var i = j - 1
        while (i > 0 && !com(arr(i - 1), key)) {
          arr(i) = arr(i - 1)
          i = i - 1
        }
        arr(i) = key
      }
      arr
    }
  }

}
