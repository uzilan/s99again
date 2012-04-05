import annotation.tailrec

class WorkingWithLists {

  def last(l: List[Int]): Int = l.last

  def lastRecursive(l: List[Int]): Int = l match {
    case e :: Nil => e
    case _ :: tail => lastRecursive(tail)
    case _ => throw new IllegalArgumentException
  }

  def penultimate(l: List[Int]): Int = l.dropRight(1).last

  def penultimateRecursive(l: List[Int]): Int = l match {
    case e :: Nil => throw new IllegalArgumentException
    case e :: f :: Nil => e
    case _ :: tail => penultimateRecursive(tail)
    case _ => throw new IllegalArgumentException
  }

  def nth(index: Int, l: List[Int]): Int = {
    if (index < 0 || index > l.size) throw new IllegalArgumentException
    else l(index)
  }

  def nthRecursive(index: Int, l: List[Int]): Int = (index, l) match {
    case (0, e :: _) => e
    case (i, e :: tail) => nthRecursive(i - 1, tail)
    case (_, _) => throw new IllegalArgumentException
  }

  def length[T](l: List[T]): Int = l.size

  def lengthRecursive[T](l: List[T]): Int = {
    def lengthRecursiveHelper[T](l: List[T], count: Int): Int = (l, count) match {
      case (Nil, i) => i
      case (e :: tail, i) => lengthRecursiveHelper(tail, i + 1)
    }
    lengthRecursiveHelper(l, 0)
  }

  def lengthFunctional[T](l: List[T]): Int = {
    l.foldLeft(0) {
      (count, _) => count + 1
    }
  }

  def reverse[T](l: List[T]): List[T] = l.reverse

  def reverseRecursive[T](l: List[T]): List[T] = l match {
    case e :: tail => reverseRecursive(tail) :+ e
    case _ => Nil
  }

  def reverseFunctional[T](l: List[T]): List[T] = l.foldLeft(List[T]()) {
    (result, e) => e +: result
  }

  //@tailrec
  def isPalindrome[T](l: List[T]): Boolean = {
    if (l.size < 2) true
    else if (l.head == l.last) isPalindrome(l.tail.takeRight(1)) else false
  }

  def isPalindromeQue[T](l: List[T]): Boolean = l == l.reverse

  def flatten(l: List[Any]): List[Any] = l flatMap {
    case multi: List[_] => flatten(multi)
    case element => List(element)
  }

  def compress[T](l: List[T]): List[T] = l match {
    case a :: b :: tail => if (a == b) compress(a :: tail) else a :: compress(b :: tail)
    case a => a
  }

  def compressFunctional[T](l: List[T]): List[T] = l.foldLeft(List[T]()) {
    (result, element) => {
      if (result.isEmpty || result.last != element) result :+ element
      else result
    }
  }

  def pack[T](l: List[T]): List[List[T]] = l.foldLeft(List[List[T]]()) {
    (result, element) => {
      if (result.isEmpty || result.last.last != element) result :+ List(element)
      else result.dropRight(1) :+ (result.last :+ element)
    }
  }

  def encode[T](l: List[T]): List[(Int, T)] = {
    pack(l) map {
      element => (element.size, element.head)
    }
  }

  def encodeModified[T](l: List[T]): List[Any] = {
    pack(l) map {
      element =>
        if (element.size == 1) element.head
        else (element.size, element.head)
    }
  }

  def decode[T](l: List[(Int, T)]): List[T] = l flatMap {
    (element) => List.fill(element._1)(element._2)
  }

  def encodeDirect[T](l: List[T]): List[(Int, T)] = l.foldLeft(List[List[T]]()) {
      (result, element) => {
        if (result.isEmpty || result.last.last != element) result :+ List(element)
        else result.dropRight(1) :+ (result.last :+ element)
      }
    } map {
      element => (element.size, element.head)
    }

  def duplicate[T](l: List[T]): List[T] = l flatMap {
    element => List.fill(2)(element)
  }
}
