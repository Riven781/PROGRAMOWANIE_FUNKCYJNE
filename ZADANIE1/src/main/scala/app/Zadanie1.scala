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

  /*
  3.5 Zaimplementuj funkcję, która zsumuje liczby po wierszach z trzech
  list na wejściu; wykorzystaj funkcję sprowadzi funkcję z trzema
  argumentami, do funkcji z dwoma
  */

  @cask.postJson("/sumLists")
  def sumListsRequest(list1: List[Int], list2: List[Int], list3: List[Int]) = {
    val resultList = sumLists(list1, list2, list3)
    
    ujson.Obj(
      "sumLists" -> resultList
    )
  }

  def sumLists(list1: List[Int], list2: List[Int], list3: List[Int]) : List[Int] = {
    if (list1.isEmpty || list2.isEmpty || list3.isEmpty) return Nil
    else{
      val f3 = (a: Int, b: Int, c: Int) => a + b + c
      val f2 = sumLists2(f3, list3.head)
      f2(list1.head, list2.head) :: sumLists(list1.tail, list2.tail, list3.tail)
    }
   
  }

  def sumLists2(f3: (Int, Int, Int) => Int, value3: Int) : (Int, Int) => Int = {
    (a, b) => f3(a, b, value3)
  }


  
  override def host: String = "0.0.0.0"
  initialize()
}