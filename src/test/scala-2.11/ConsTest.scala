import org.scalatest.{Matchers, GivenWhenThen, FlatSpec}

class ConsTest extends FlatSpec with GivenWhenThen with Matchers {
  it should "contain after add" in {
    val list = new Cons(5, new Cons(3, Nil))
    val result = list add 7

    assert(result contains 7)
    assert(result contains 5)
    assert(result contains 3)
    assert(!(result contains 4))
  }

  it should "contain doubled values after map" in {
    val list = new Cons(5, new Cons(3, Nil))
    val result = list map (x => x * 2)

    assert(result contains 10)
    assert(result contains 6)
    assert(!(result contains 5))
    assert(!(result contains 3))
  }

  it should "contain unique values after union" in {
    val list = new Cons(5, new Cons(3, Nil))
    val result = list union new Cons(9, new Cons(7, Nil))

    assert(result contains 9)
    assert(result contains 7)
    assert(result contains 5)
    assert(result contains 3)

  }
}
