object Nill extends IntList {
  override def contains(x: Int): Boolean = false

  override def map(function: (Int) => Int): IntList = Nill

  override def union(list: IntList): IntList = list

  override def add(i: Int): IntList = Nill
}
