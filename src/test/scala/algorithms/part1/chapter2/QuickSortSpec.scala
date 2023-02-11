package algorithms.part1.chapter2


import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QuickSortSpec extends AnyWordSpec with Matchers {

  "QuickSort.sorted" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4, 8, 3, 5, 7, 1, 2, 9, 6)
        val sortedArray = unsortedArray.sortWith(ordering)
        val actualResult = QuickSort.sort(unsortedArray)(ordering)
        actualResult shouldBe sortedArray
      }
    }

    "called for an array with exactly 1 element" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4)
        val sortedArray = unsortedArray.sortWith(ordering)
        val actualResult = QuickSort.sort(unsortedArray)(ordering)
        actualResult shouldBe sortedArray
      }
    }

    "called for an array with no element" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array.empty[Int]
        val sortedArray = unsortedArray.sortWith(ordering)
        val actualResult = QuickSort.sort(unsortedArray)(ordering)
        actualResult shouldBe sortedArray
      }
    }
  }
}