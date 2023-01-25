package part1.chapter2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeSpec extends AnyWordSpec with Matchers {

  "ArraySorting.inSorted" when {
    "called for an array with more than 1 elements" should {
      "work correctly" in {

        val tree = Branch(Leaf(1), Leaf(2))
        val f: (String, Int) => String = (s, i) => s + (i.toString * i)

        val actualValue = tree.fold("")(f)
        val expectedValue = "122"

        expectedValue shouldBe actualValue
      }

      "work correctly2" in {

        val tree = Leaf(7)
        val f: (String, Int) => String = (s, i) => s + (i.toString * i)

        val actualValue = tree.fold("")(f)
        val expectedValue = "7777777"

        expectedValue shouldBe actualValue
      }

      "work correctly3" in {

        val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
        val f: (String, Int) => String = (s, i) => s + (i.toString * i)

        val actualValue = tree.fold("")(f)
        val expectedValue = "122333"

        expectedValue shouldBe actualValue
      }

      "work correctly4" in {

        val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
        val f: (String, Int) => String = (s, i) => s + (i.toString * i)

        val actualValue = tree.foldTail("")(f)
        val expectedValue = "122333444455555"

        actualValue shouldBe expectedValue
      }
    }
  }

}
