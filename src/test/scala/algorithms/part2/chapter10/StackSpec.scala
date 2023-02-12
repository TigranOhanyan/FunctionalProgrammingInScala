package algorithms.part2.chapter10


import algorithms.part2.{Overflow, Underflow}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class StackSpec extends AnyWordSpec with Matchers {

  "Stack" when {
    "called insert" should {
      "puts an element into it, if there is a place" in {
        val stack = new Stack[Int](7)
        val elem = 88
        stack.push(elem)
        stack.top shouldBe 1
        stack.array shouldBe Array(88, 0, 0, 0, 0, 0, 0)
      }

      "throws exception if stack is full" in {
        val stack = new Stack[Int](1)
        val elem = 88
        stack.push(elem)

        Try(stack.push(elem)).failed.toOption should contain(Overflow)
      }
    }

    "called pop" should {
      "gets the latest inserted element if there is one" in {
        val stack = new Stack[Int](7)
        val elem = 88
        stack.push(elem)
        stack.push(elem + 1)
        stack.push(elem + 2)
        val expectedResult = 100
        stack.push(expectedResult)
        val actualResult = stack.pop()
        expectedResult shouldBe actualResult
        stack.top shouldBe 3
        stack.array shouldBe Array(88, 89, 90, 100, 0, 0, 0)
      }

      "throws exception if stack is empty" in {
        val stack = new Stack[Int](7)
        Try(stack.pop()).failed.toOption should contain(Underflow)
      }
    }
  }
}