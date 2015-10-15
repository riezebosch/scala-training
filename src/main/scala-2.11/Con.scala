class Con(val x: Int, xs: IntList) extends IntList{
  def union(list: IntList): IntList = {
    xs union (list add x)
  }

  def map(function: (Int) => Int): IntList = {
    new Con(function(x), xs map function)
  }

  def contains(i: Int): Boolean = {
    if (i == x)
      true
    else
      xs contains i
  }

  def add(i: Int): IntList = {
    if (!contains(i))
      new Con(i, this)
    else
      this
  }

}
