package algorithms.part1.chapter2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MergeSortSpec extends AnyWordSpec with Matchers {

  "MergeSort.Mutable.sorted" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4,8,3,5,7,1,2,9,6)
        val sortedArray = unsortedArray.sortWith(ordering)
        MergeSort.Mutable.sort(unsortedArray, ordering)
        unsortedArray shouldBe sortedArray
      }
    }
  }


  "MergeSort.Immutable.sorted" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4, 8, 3, 5, 7, 1, 2, 9, 6)
        val expectedSortedArray = unsortedArray.sortWith(ordering)
        val actualSortedArray = MergeSort.Immutable.sort(unsortedArray, ordering)
        actualSortedArray shouldBe expectedSortedArray
      }
    }
  }
}
