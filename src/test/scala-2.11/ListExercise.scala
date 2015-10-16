import org.scalatest.{Matchers, GivenWhenThen, FlatSpec}

class ListExercise extends FlatSpec with GivenWhenThen with Matchers {
  val namen = List("Sylvia", "Jeanette", "Natalie", "Fiene",
    "Elsje", "Treesje", "Truus", "Babbette", "Betsie", "Sabine",
    "Greet", "Magreet", "Magriet", "Marie", "Marije", "Angeline",
    "Wies", "Marjan", "Marjo", "Marlyn", "Tine" )

  it should "tell me the number of names" in {
    val length = namen.:\(0) ((x, n) => n+1)
    assert(length == 21)
  }

  it should "tell me the length of all names" in {
    val length = namen map (_.length)
    length foreach (println _)
  }

  it should "tell me the total length" in {
    val length = (namen map (_.length)).:\(0)(_+_)
    assert(length == 126)
  }

  it should "seperate list starting with an 'M'" in {
    val notm = namen filter(!_.startsWith("M"))
    val m = namen.filterNot(notm.toSet)

    println("not-m ---")
    notm foreach (println _)

    println("m -------")
    m foreach (println _)
  }

  it should "group names by length" in {
    val group: List[(Int, List[String])] = namen.groupBy(_.length).toList
//    val max =
//
//    println(s"meest voorkomend: $max")
  }
}
