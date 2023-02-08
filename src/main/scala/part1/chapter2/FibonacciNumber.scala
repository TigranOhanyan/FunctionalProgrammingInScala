package part1.chapter2

import scala.annotation.tailrec

object FibonacciNumber {

  // 2.1
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 1) + fib(n - 2)
  }

  def fibRec(n: Int): Int = {
    @tailrec
    def loop(`n-1`: Int, `n-2`: Int, index: Int): Int =
      index match {
        case `n` => `n-1` + `n-2`
        case index => loop(`n-1` + `n-2`, `n-1`, index + 1)
      }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => loop(1, 0, 2)
    }
  }

}
