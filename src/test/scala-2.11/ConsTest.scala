import org.scalatest.{WordSpec, FunSpec, Matchers, FlatSpec}
import List._

class ConsTest extends WordSpec with Matchers {
  "A list" when {
    "adding" should {
      "contain new item" in {
        val list = Cons(5, Cons(3, Nil))
        val result = list add 7

        result contains 7 should equal (true)
        result contains 5 should equal (true)
        result contains 3 should equal (true)
        result contains 4 should equal (false)
      }

      "add at the end" in {
        val list = Cons(3, Nil)
        val result = list add 5

        result.toString should equal("3 - 5")
      }

      "contain 1 after add on Nil" in {
        val list = Nil add 1

        list.toString should equal("1")
      }
    }

    "mapping" should {
      "contain doubled values after map" in {
        val list = Cons(5, Cons(3, Nil))
        val result = list map (x => x * 2)

        result contains 10 should equal(true)
        result contains 6 should equal(true)
        result contains 5 should not equal true
        result contains 3 should not equal true
      }
    }

    "union" should {
      "contain values after union" in {
        val list = Cons(5, Cons(3, Nil))
        val result = list union Cons(9, Cons(7, Nil))

        result contains 9 should equal(true)
        result contains 7 should equal(true)
        result contains 5 should equal(true)
        result contains 3 should equal(true)
      }

      "contain unique values after union" in {
        val list1 = Cons(5, Nil)
        val list2 = Cons(5, Nil)
        val result = list1 union list2

        result.toString should equal("5")
      }

      "contain original values after union" in {
        val list1 = Cons(3, Cons(3, Nil))
        val list2 = Cons(5, Nil)
        val result = list1 union list2

        result.toString should equal("3 - 3 - 5")
      }

      "contain duplicates from both lists" in {
        val list1 = Cons(3, Cons(3, Nil))
        val list2 = Cons(5, Cons(5, Nil))

        val result = list1 union list2

        result.toString should equal("3 - 3 - 5 - 5")
      }
    }

    "converting to string" should {

      "write empty for an empty list" in {
        Nil.toString() should equal("empty list")
      }

      "write 1 for a list of one item" in {
        Cons(1, Nil).toString() should equal("1")
      }

      "write 1 - 2 for a list of two items" in {
        Cons(1, Cons(2, Nil)).toString() should equal("1 - 2")
      }
    }

    "adding with the :: operator" should {
      "concatenate items with the :: operator" in {
        val list = Cons(1, Nil).::(1)
        list.toString() should equal("1 - 1")
      }

      "implicit convert items with the :: operator" in {
        val list: List[Int] = 1 :: 2
        list.toString() should equal("1 - 2")
      }

      "concatenate multiple values in right order" in {
        val list: List[Int] = 1 :: 2 :: 3
        list.toString should equal("1 - 2 - 3")
      }
    }

    "reverse" should {
      "be empty when reverse empty" in {
        Nil reverse() should equal(Nil)
      }

      "be same when single item" in {
        val list = Cons(1, Nil)
        val result = list reverse()

        result.toString should equal("1")
      }

      "be reversed for more items" in {
        val list = Cons(1, Cons(2, Nil))
        val result = list reverse()

        result.toString should equal("2 - 1")
      }
    }
  }
}
