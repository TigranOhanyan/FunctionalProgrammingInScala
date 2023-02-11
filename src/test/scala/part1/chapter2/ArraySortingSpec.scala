package part1.chapter2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArraySortingSpec extends AnyWordSpec with Matchers {

  "ArraySorting.isSorted" when {
    "called for an array with more than 1 elements" should {
      "return false if array is sorted" in {
        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4, 8, 3, 5, 7, 1, 2, 9, 6)
        ArraySorting.isSorted(unsortedArray, ordering) shouldBe false
      }

      "return true if array is sorted" in {
        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(1, 2, 3, 4, 5, 6, 7, 8)
        ArraySorting.isSorted(unsortedArray, ordering) shouldBe true
      }
    }

    "called for an array with less than or equal to 1 elements" should {
      "return always return true" in {
        val ordering: (Int, Int) => Boolean = _ <= _
        ArraySorting.isSorted(Array(1), ordering) shouldBe true
        ArraySorting.isSorted(Array.empty[Int], ordering) shouldBe true
      }
    }
  }
}
