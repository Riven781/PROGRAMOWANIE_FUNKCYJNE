package app
object Zadanie1 extends cask.MainRoutes{

  /*
  3.0 Zaimplementuj funkcję isSorted, która sprawdza czy dana funkcja
  jest posortowana zgodnie z podaną funkcją porównawczą
  */


  @cask.postJson("/isSorted")
  def isSortedRequest(array: Seq[Int], option: String) = {
    val compareMap = Map[String, (Int, Int) => Boolean](
      "asc" -> ((a: Int, b: Int) => a <= b),
      "desc" -> ((a: Int, b: Int) => a >= b),
      "absAsc" -> ((a: Int, b: Int) => a.abs <= b.abs),
      "absDesc" -> ((a: Int, b: Int) => a.abs >= b.abs)
    )

    if (!compareMap.contains(option)) {
      ujson.Obj(
        "error" -> "Invalid option"
      )
    } 
    else{
      ujson.Obj(
        "isSorted" -> isSorted(array, compareMap(option))
      )
    }


  }

  def isSorted(array: Seq[Int], compare: (Int, Int) => Boolean) : Boolean = {
    (0 until array.size - 1).forall(i => compare(array(i), array(i + 1)))
  }

 


  override def host: String = "0.0.0.0"
  initialize()
}