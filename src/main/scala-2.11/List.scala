import scala.annotation.tailrec

abstract class List[+T] {
  def add[SuperT >: T](n: SuperT): List[SuperT] = this match {
    case Nil => Cons(n, Nil)
    case Cons(x, xs) => Cons(x, xs add n)
  }

  def union[SuperT >: T](list: List[SuperT]): List[SuperT] = this match {
    case Nil => list
    case Cons(n, xs) => if (list contains n) list union xs else Cons(n, xs union list)
  }

  def map[U](f: T => U): List[U] = this match {
    case Nil => Nil
    case Cons(n, xs) => new Cons(f(n), xs map f)
  }

  @tailrec
  final def contains[SuperT >: T](n: SuperT): Boolean = this match {
    case Nil => false
    case Cons(x, xs) => x == n || (xs contains n)
  }

  override def toString: String = this match {
    case Nil => "empty list"
    case Cons(x, Nil) => x.toString
    case Cons(x, xs) => s"${x.toString} - ${xs.toString}"
  }


  def ::[SuperT >: T](n: SuperT): List[SuperT] = Cons(n, this)

  def reverse(): List[Any] = this match {
    case Nil => Nil
    case Cons(x, Nil) => this
    case Cons(x, xs) => xs reverse() add x

  }
}

object List {
  implicit def TToCons[T](value: T): List[T] = Cons(value, Nil)
}
