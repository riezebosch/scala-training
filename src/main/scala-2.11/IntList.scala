abstract class IntList {
  def add(i: Int): IntList

  def union(list: IntList): IntList

  def map(function: (Int) => Int): IntList

  def contains(x: Int): Boolean

}
