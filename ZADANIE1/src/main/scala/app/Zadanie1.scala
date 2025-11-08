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
    if (list1.isEmpty || list2.isEmpty || list3.isEmpty) return scala.Nil
    else{
      val f3 = (a: Int, b: Int, c: Int) => a + b + c
      val f2 = sumLists2(f3, list3.head)
      f2(list1.head, list2.head) :: sumLists(list1.tail, list2.tail, list3.tail)
    }
   
  }

  def sumLists2(f3: (Int, Int, Int) => Int, value3: Int) : (Int, Int) => Int = {
    (a, b) => f3(a, b, value3)
  }


  /*
  4.0 Zaimplementuj funkcję setHead, która doda element na początku
  listy
  */

  @cask.postJson("/setHead")
  def setHeadRequest(list: List[Int], value: Int) = {
    ujson.Obj(
      "listWithNewHead" -> setHead(list, value)
    )
  }

  def setHead(list: List[Int], value: Int) : List[Int] = {
    value :: list
  }

  /*
  4.5 Zaimplementuj funkcję append, która doda element we wskazanym
  miejscu w liście
  */

  @cask.postJson("/append")
  def appendRequest(list: List[Int], value: Int, index: Int) = {
    ujson.Obj(
      "listWithNewElement" -> append(list, value, index)
    )
  }

  def append(list: List[Int], value: Int, index: Int) : List[Int] = {
    if (index == 0 || list.isEmpty) {
      value :: list
    }
    else{
      list.head :: append(list.tail, value, index - 1)
    } 
  }


  /*
  5.0 Zaimplementuj funkcję, która policzy kwadrat liczb z dwóch list
  (po wierszach) za pomocą funkcji map oraz funkcji anonimowej
  */

  @cask.postJson("/squareLists")
  def squareListsRequest(list1: List[Int], list2: List[Int]) = {
    ujson.Obj(
      "squareLists" -> squareLists(list1, list2)
    )
  }


  def squareLists(list1: List[Int], list2: List[Int]) : List[Int] = {
    list1.zip(list2).flatMap(x => List(x._1 * x._1, x._2 * x._2))
  }


  //ZADANIE 3

  sealed trait Lista[+A]
  case object Nil extends Lista[Nothing]
  case class Node[+A](head: A, tail: Lista[A]) extends  Lista[A]


  object Lista{
    def apply[A](as: A*): Lista[A] =
      if (as.isEmpty) Nil
      else Node(as.head, apply(as.tail: _*))

    
    //A to typ elementów listy a B to typ akumulatora
    def foldLeft[A, B](lista: Lista[A], acc: B, f: (B, A) => B) : B =
      lista match
        case Nil => acc
        case node : Node[A] => foldLeft(node.tail, f(acc, node.head), f)
  }


  sealed trait DLista[+A]
  case object DNil extends DLista[Nothing]
  case class DNode[A](val prev: DLista[A], var next: DLista[A], val value: A) extends DLista[A]

  
  object DLista {
    def apply[A](as: A*): DLista[A] =
      if (as.isEmpty) DNil
      else build(DNil, as.toList)

    private def build[A](prev: DLista[A], tail: List[A]): DNode[A] = tail match {
      case h :: scala.Nil =>
        DNode(prev, DNil, h)
      case h :: t =>
        val current = DNode(prev, DNil, h)
        val next  = build(current, t)
        current.next = next
        current
    }
  }


  def fromScalaListD[A](l: List[A]) : DLista[A] = DLista(l: _*)

  def toScalaListD[A](l: DLista[A]) : List[A] = l match
    case DNil => scala.Nil
    case DNode(prev, next, value) => value :: toScalaListD(next)

  
  def toScalaList[A](l: Lista[A]) : List[A] = l match
    case Nil => scala.Nil
    case Node(head, tail) => head :: toScalaList(tail)

  def fromScalaList[A](l: List[A]) : Lista[A] = l match
    case scala.Nil => Nil
    case head :: tail => Node(head, fromScalaList(tail))

  /*
  3.0 zwrócą wynik funkcji tail, która usuwa pierwszy element z listy
  (parameter); należy rozważyć przypadek Nil jako parametr
  */

  @cask.postJson("/tail")
  def tailRequest(list: List[Int]) = {
    val lista = fromScalaList(list)
    val result = tail(lista)
    
    ujson.Obj(
      "tail" -> toScalaList(result)
    )
  }

  def tail[A](lista: Lista[A]) : Lista[A] =
    lista match
      case Nil => Nil
      case Node(_,tail_list) => tail_list

  /*
  3.5 zwróci wynik funkcji drop, która usuwa n elementów z listy
  dwukierunkowej*/
  @cask.postJson("/drop")
  def dropRequest(list: List[Int], n: Int) = {
    val lista = fromScalaListD(list)
    val result = drop(lista, n)
    
    ujson.Obj(
      "resultList" -> toScalaListD(result)
    )
  }

  def drop[A](dLista: DLista[A], n : Int) : DLista[A]=
    if (n <= 0) dLista
    else
      dLista match
        case DNil => DNil
        case DNode(_, next, _) => drop(next, n - 1)

  /*
  4.0 zwróci wynik funkcji dropWhile, która usuwa n elementów z listy
  dwukierunkowej, które spełniają warunek funkcji (parametr); należy
  wykorzystać podejście pattern match
  */

  @cask.postJson("/dropWhile")
  def dropWhileRequest(list: List[Int], option: String) = {
    val lista = fromScalaListD(list)
    val result = option match
      case "even" => dropWhile(lista, _ % 2 == 0)
      case "odd" => dropWhile(lista, _ % 2 != 0)
      case _ => DNil
    
    ujson.Obj(
      "resultList" -> toScalaListD(result)
    )
  }

  def dropWhile[A](dLista: DLista[A], f: A => Boolean): DLista[A] =
    dLista match
      case DNil => DNil
      case node : DNode[Int] => if (f(node.value)) dropWhile(node.next, f) else node

  /*
  4.5 zwróci wynik funkcji foldLeft wykorzystując do tego companion
  object
  */

  @cask.postJson("/foldLeft")
  def foldLeftRequest(list: List[Int], option: String) = {
    val lista = fromScalaList(list)
    val result = option match
      case "sum" => Lista.foldLeft(lista, 0, _ + _)
      case "product" => Lista.foldLeft(lista, 1, _ * _)
      case _ => 0
    
    
    ujson.Obj(
      "result" -> result
    )
  }

  /*
  5.0 zwróci wynik funkcji concatenate na dwóch listach (parametry),
  która zwraca jedną listę
  */

  @cask.postJson("/concatenate")
  def concatenateRequest(list1: List[Int], list2: List[Int]) = {
    val lista1 = fromScalaList(list1)
    val lista2 = fromScalaList(list2)
    val result = concatenate(lista1, lista2)
    
    
    ujson.Obj(
      "result" -> toScalaList(result)
    )
  }

  def concatenate(lista1: Lista[Int], lista2: Lista[Int]) : Lista[Int] =
    lista1 match
      case Nil => lista2
      case Node(head, tail) => Node(head, concatenate(tail, lista2))






    

  override def host: String = "0.0.0.0"
  initialize()
}