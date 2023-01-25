package part1.chapter2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FibonacciNumberSpec extends AnyWordSpec with Matchers {

  "fib" when {
    "called for numbers larger than 1" should {
      "work correctly" in {
        val n = 11
        val `fib(n)` = 89
        val actualResult = FibonacciNumber.fib(n)
        actualResult shouldBe `fib(n)`
      }
    }

    "called for 0" should {
      "work correctly" in {
        val n = 0
        val `fib(n)` = 0
        val actualResult = FibonacciNumber.fib(n)
        actualResult shouldBe `fib(n)`
      }
    }

    "called for 1" should {
      "work correctly" in {
        val n = 1
        val `fib(n)` = 1
        val actualResult = FibonacciNumber.fib(n)
        actualResult shouldBe `fib(n)`
      }
    }
  }

  "fibRec" when {
    "called for numbers larger than 1" should {
      "work correctly" in {
        val n = 11
        val `fib(n)` = 89
        val actualResult = FibonacciNumber.fibRec(n)
        actualResult shouldBe `fib(n)`
      }
    }

    "called for 0" should {
      "work correctly" in {
        val n = 0
        val `fib(n)` = 0
        val actualResult = FibonacciNumber.fibRec(n)
        actualResult shouldBe `fib(n)`
      }
    }

    "called for 1" should {
      "work correctly" in {
        val n = 1
        val `fib(n)` = 1
        val actualResult = FibonacciNumber.fibRec(n)
        actualResult shouldBe `fib(n)`
      }
    }
  }

}
