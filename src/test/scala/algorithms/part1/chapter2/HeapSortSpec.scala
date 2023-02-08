package algorithms.part1.chapter2


import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HeapSortSpec extends AnyWordSpec with Matchers {

  "HeapSort.sort" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4, 8, 3, 5, 7, 1, 2, 9, 6)
        val sortedArray = unsortedArray.sortWith(ordering)
        HeapSort.sort(unsortedArray)(ordering)
        unsortedArray shouldBe sortedArray
      }
    }
  }


  "HeapSort.sorted" when {
    "called for an array with 1 element" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4)
        val expectedSortedArray = unsortedArray.sortWith(ordering)
        HeapSort.sort(unsortedArray)(ordering)
        unsortedArray shouldBe expectedSortedArray
      }
    }
  }
}
