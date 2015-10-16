import org.scalatest.{Matchers, GivenWhenThen, FlatSpec}

class ConsTest extends FlatSpec with GivenWhenThen with Matchers {
  it should "contain after add" in {
    val list = Cons(5, Cons(3, Nil))
    val result = list add 7

    assert(result contains 7)
    assert(result contains 5)
    assert(result contains 3)
    assert(!(result contains 4))
  }

  it should "not contain same value twice after add" in {
    val list = Cons(5, Nil)
    val result = list add 5
    assert(list == result)
  }

  it should "contain doubled values after map" in {
    val list = Cons(5, Cons(3, Nil))
    val result = list map (x => x * 2)

    assert(result contains 10)
    assert(result contains 6)
    assert(!(result contains 5))
    assert(!(result contains 3))
  }

  it should "contain values after union" in {
    val list = Cons(5, Cons(3, Nil))
    val result = list union Cons(9, Cons(7, Nil))

    assert(result contains 9)
    assert(result contains 7)
    assert(result contains 5)
    assert(result contains 3)
  }

  it should "contain unique values after union" in {
    val list1 = Cons(5, Nil)
    val list2 = Cons(5, Nil)
    val result = list1 union list2

    assert(result.toString == "5")
  }

  it should "contain original values after union" in {
    val list1 = Cons(3, Cons(3, Nil))
    val list2 = Cons(5, Nil)
    val result = list1 union list2

    assert(result.toString == "3 - 3 - 5")
  }

  it should "write empty for an empty list" in {
    assert(Nil.toString() == "empty list")
  }

  it should "write 1 for a list of one item" in {
    assert(Cons(1, Nil).toString() == "1")
  }

  it should "write 1 - 2 for a list of two items" in {
    assert(Cons(1, Cons(2, Nil)).toString() == "1 - 2")
  }
}
