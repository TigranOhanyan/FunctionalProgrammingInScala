package algorithms.part2.chapter10

import algorithms.part2.{Overflow, Underflow}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Try}

class QueueSpec extends AnyWordSpec with Matchers {

  "Queue" when {
    "called enqueue" should {
      "puts an element into it, if there is a place" in {
        val queue = new Queue[Int](7)
        val elem = 88
        queue.enqueue(elem)
        queue.head shouldBe 1
        queue.tail shouldBe 2
        queue.array shouldBe Array(88, 0, 0, 0, 0, 0, 0, 0)
      }

      "throws exception if queue is full" in {
        val queue = new Queue[Int](2)
        val elem = 88
        queue.enqueue(elem)
        queue.enqueue(elem)

        Try(queue.enqueue(elem)) shouldBe Failure(Overflow)
      }
    }

    "called dequeue" should {
      "gets the earliest inserted element if there is one" in {
        val queue = new Queue[Int](7)
        val expectedResult = 100
        queue.enqueue(expectedResult)
        val elem = 88
        queue.enqueue(elem)
        queue.enqueue(elem + 1)
        queue.enqueue(elem + 2)

        val actualResult = queue.dequeue()
        expectedResult shouldBe actualResult
        queue.head shouldBe 2
        queue.tail shouldBe 5
        queue.array shouldBe Array(100, 88, 89, 90, 0, 0, 0, 0)
      }

      "throws exception if stack is empty" in {
        val queue = new Queue[Int](7)
        Try(queue.dequeue()) shouldBe Failure(Underflow)
      }
    }
  }
}
