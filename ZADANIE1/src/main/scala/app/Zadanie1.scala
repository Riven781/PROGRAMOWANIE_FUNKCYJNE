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
      case node : DNode[A] => if (f(node.value)) dropWhile(node.next, f) else node

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


  //ZADANIE 4

  /*
  3.0 zwróci taki sam wynik jak zad.1 4.5 ale wykorzysta typ opcjonalny
  */

  @cask.postJson("/appendOptional")
  def appendRequestOptoional(list: List[Int], value: Int, index: Int) = {
    val result = appendOptional(list, value, index)
    result match
      case Some(x) => ujson.Obj(
        "listWithNewElement" -> x
      )
      case None => ujson.Obj(
        "message" -> "Index < 0"
      )
  }

  def appendOptional(list: List[Int], value: Int, index: Int) : Option[List[Int]] = {  //None wtedy gdy indeks jest ujemny, gdy jest wiekszy od dlugosci listy to na koniec dajemy wartosc
    if index < 0 then None
    else 
      if (index == 0 || list.isEmpty) {
        Some(value :: list)
      }
      else{
        appendOptional(list.tail, value, index - 1).map(t => list.head :: t)
      } 
  }


  /*
  3.5 zwroci wariancję z listy; do implementacji wykorzystać należy typ
  opcjonalny
  */

  @cask.postJson("/variance")
  def varianceRequest(list: List[Int]) = {
    val result = variance(list)
    result match 
      case Some(x) => ujson.Obj(
        "variance" -> x
      )
      case None => ujson.Obj(
        "message" -> "List is empty"
      )

  }

  def variance(list: List[Int]) : Option[Double] = 
    list match
      case scala.Nil => None
      case _ => 
        val length = list.length.toDouble
        val mean = list.sum / length
        val variance = list.map(x => (x - mean) * (x - mean)).sum / length
        Some(variance)

  /*
  4.0 zwróci listę (option), która jest wynikiem połączenia dwóch list
  (option); w przypadku gdy jedna z list jest None, funkcja powinna
  zwrócić None
  */

  @cask.postJson("/concatenateOptional")
  def concatenateOptionalRequest(list1: List[Int], list2: List[Int]) = {
    val result = concatenateOptional(list1, list2)
    result match 
      case Some(x) => ujson.Obj(
        "List" -> x
      )
      case None => ujson.Obj(
        "message" -> "List is empty"
      )
  }

  def concatenateOptional(list: List[Int], list2: List[Int]): Option[List[Int]] =
    if (list.isEmpty || list2.isEmpty) None
    else Some(list ++ list2)

  /*
  4.5 zwróci listę za pomocą metody mojeMap[A,B,C](a: Option[A], b:
  Option[B])(f:(A,B)=>C):Option[C]; należy wykorzystać metodę flatMap
  oraz map
  */

  @cask.postJson("/mojeMap")
  def mojeMapRequest(arg1: Option[Int], arg2: Option[Int], f: String) = {
    val fun : (Int, Int) => Int = f match
      case "sum" => _ + _
      case "product" => _ * _
      case "minus" => _ - _
      case _ => (a,b) => a - b
    
    val result = mojeMap(arg1, arg2)(fun)
    result match 
      case Some(x) => ujson.Obj(
        "Result" -> x
      )
      case None => ujson.Obj(
        "message" -> "None"
      )
  }

  def mojeMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))


    //flatMap spłaszcza Option, zeby w tym wypadku nie bylo zagniezdzonego Option



  /*
  5.0 zwróci taki średnią (mean) z listy zwracając Either
  */

  @cask.postJson("/mean")
  def meanRequest(list: List[Int]) = {
    
    val result = getMean(list)
    result match 
      case Right(x) => ujson.Obj(
        "Mean" -> x
      )
      case Left(x) => ujson.Obj(
        "message" -> x
      )
  }

  def getMean(list: List[Int]) : Either[String, Double] = 
    if (list.isEmpty) Left("List is empty")
    else Right(list.sum / list.length.toDouble)


  /*ZADANIE 5*/

  case class KeyValue[K, V](key: K, value: V)

  trait MapReduce[In, Key, Value, Reduced] {
    def mapper(input: In): Seq[KeyValue[Key, Value]]
    def reducer(key: Key, values: Seq[Value]): KeyValue[Key, Reduced]
  }

  /*
  3.0 zwróci słownik z liczbą powtarzających się liczb za pomocą funkcji
  mapreduce dla list z liczbami
  */

  class NumberCount extends MapReduce[Int, Int, Int, Int] {
    override def mapper(input: Int): Seq[KeyValue[Int, Int]] = {
      Seq(KeyValue(input, 1))  //kazde wystapienie mapujemy na 1
    }

    override def reducer(key: Int, values: Seq[Int]): KeyValue[Int, Int] = {  //key
      KeyValue(key, values.sum)   
    }
  }
  object MapReduce{
    def mapReduce[In, Key, Value, Reduced](input: Seq[In], mr: MapReduce[In, Key, Value, Reduced]): Seq[KeyValue[Key, Reduced]] = {
      val mapped = input.flatMap(mr.mapper)
      val grouped : Seq[KeyValue[Key, Seq[Value]]] = mapped.groupBy(_.key).map {case (key, keyValuesSeq) => KeyValue(key, keyValuesSeq.map(_.value)) }.toSeq
      grouped.map(keyValue =>  mr.reducer(keyValue.key, keyValue.value))
    }
  }

  @cask.postJson("/numberCount")
  def NumberCountRequest(list: List[Int]) = {
    
    val result = MapReduce.mapReduce(list, new NumberCount())
    ujson.Obj(
      "numberCount" -> result.map(kv => ujson.Obj("number" -> kv.key, "count" -> kv.value))
    )
  }

  /*
  3.5 zwróci słownik z liczbami podniesionymi do sześcianu za pomocą
  funkcji mapreduce dla list z liczbami
  */

  class NumberPow extends MapReduce[Int, Int, Int, Int] {
    override def mapper(input: Int): Seq[KeyValue[Int, Int]] = {
      Seq(KeyValue(input, input))
    }

    override def reducer(key: Int, values: Seq[Int]): KeyValue[Int, Int] = {  //key
      KeyValue(key, key * key * key) //inna opcja to values.head * values.head * values.head
    }
  }

  @cask.postJson("/numberPow")
  def NumberPowRequest(list: List[Int]) = {
    
    val result = MapReduce.mapReduce(list, new NumberPow())
    ujson.Obj(
      "numberPow" -> result.map(kv => ujson.Obj("number" -> kv.key, "numberPow(3)" -> kv.value))
    )
  }











  override def host: String = "0.0.0.0"
  initialize()
}