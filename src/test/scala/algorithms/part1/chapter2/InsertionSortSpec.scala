package algorithms.part1.chapter2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InsertionSortSpec extends AnyWordSpec with Matchers {

  "InsertionSort.Mutable.sort" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4, 8, 3, 5, 7, 1, 2, 9, 6)
        val sortedArray = unsortedArray.sortWith(ordering)
        InsertionSort.Mutable.sort(unsortedArray)(ordering)
        unsortedArray shouldBe sortedArray
      }
    }
  }


  "InsertionSort.Immutable.inSorted" when {
    "called for an array with 1 element" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4)
        val expectedSortedArray = unsortedArray.sortWith(ordering)
        val actualSortedArray = InsertionSort.Mutable.sort(unsortedArray)(ordering)
        actualSortedArray shouldBe expectedSortedArray
      }
    }
  }
}
