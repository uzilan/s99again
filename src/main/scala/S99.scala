import annotation.tailrec

object S99 {

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

  @tailrec
  def isPalindrome[T](l: List[T]): Boolean = {
    if (l.size < 2) true
    else if (l.head == l.last) isPalindrome(l.tail.takeRight(1)) else false
  }

  def isPalindromeQue[T](l: List[T]): Boolean = l == l.reverse

}
