import scala.annotation.tailrec

abstract class IntList {
  def add(n: Int): IntList = {
    if (!contains(n)) new Cons(n, this)
    else this
  }

  def union(list: IntList): IntList = this match {
    case Nil => list
    case Cons(n, xs) =>  xs union (list add n)
  }

  def map(function: (Int) => Int): IntList = this match {
    case Nil => Nil
    case Cons(n, xs) => new Cons(function(n), xs map function)
  }

  @tailrec
  final def contains(n: Int): Boolean = this match {
    case Nil => false
    case Cons(x, xs) => x == n || (xs contains n)
  }

  override def toString: String = this match {
    case Nil => "empty list"
    case Cons(x, Nil) => x.toString
    case Cons(x, xs) => s"${x.toString} - ${xs.toString}"
  }
}
