package part1.chapter2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArraySortingSpec extends AnyWordSpec with Matchers {

  "ArraySorting.inSorted" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val ordering: (Int, Int) => Boolean = _ <= _
        val unsortedArray = Array(4,8,3,5,7,1,2,9,6)
        val sortedArray = unsortedArray.sortWith(ordering)
        ArraySorting.Mutable.inSorted(unsortedArray, ordering)
        unsortedArray shouldBe sortedArray
      }
    }
  }

}
