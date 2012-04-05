import org.specs2.mutable.Specification

class S99Spec extends Specification {

  "S99" should {
    /*
   P01 (*) Find the last element of a list.
  Example:
  scala> last(List(1, 1, 2, 3, 5, 8))
  res0: Int = 8
    */

    "find last element" in {
      val last = S99.last(List(1, 1, 2, 3, 5, 8))
      last mustEqual 8
    }
    "find last element recursively" in {
      val last = S99.lastRecursive(List(1, 1, 2, 3, 5, 8))
      last mustEqual 8
    }

    /*
   P02 (*) Find the last but one element of a list.
 Example:
 scala> penultimate(List(1, 1, 2, 3, 5, 8))
 res0: Int = 5
    */

    "find the last but one element" in {
      val lastButOne = S99.penultimate(List(1, 1, 2, 3, 5, 8))
      lastButOne mustEqual 5
    }

    "find the last but one element recursively" in {
      val lastButOne = S99.penultimateRecursive(List(1, 1, 2, 3, 5, 8))
      lastButOne mustEqual 5
    }

    /*
    P03 (*) Find the Kth element of a list.
  By convention, the first element in the list is element 0.
  Example:

  scala> nth(2, List(1, 1, 2, 3, 5, 8))
  res0: Int = 2
    */
    "find the Kth element of a list" in {
      val nth = S99.nth(2, List(-1, 0, 1, 2, 3, 5, 8))
      nth mustEqual 1
    }

    "find the Kth element of a list with invalid index" in {
      S99.nth(-1, List(-1, 0, 1, 2, 3, 5, 8)) must throwAn[IllegalArgumentException]
    }

    "find the Kth element of a list recursively" in {
      val nth = S99.nthRecursive(2, List(-1, 0, 1, 2, 3, 5, 8))
      nth mustEqual 1
    }

    "find the Kth element of a list with invalid index recursively" in {
      S99.nthRecursive(-1, List(-1, 0, 1, 2, 3, 5, 8)) must throwAn[IllegalArgumentException]
      S99.nthRecursive(20, List(-1, 0, 1, 2, 3, 5, 8)) must throwAn[IllegalArgumentException]
    }

    /*
    P04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
    */
    "find the length of the list" in {
      S99.length(List(1, 1, 2, 3, 5, 8)) mustEqual 6
    }

    "find the length of the list recursively" in {
      S99.lengthRecursive(List(1, 1, 2, 3, 5, 8)) mustEqual 6
      S99.lengthRecursive(List()) mustEqual 0
    }

    "find the length of the list functionally" in {
      S99.lengthFunctional(List(1, 1, 2, 3, 5, 8)) mustEqual 6
      S99.lengthFunctional(List()) mustEqual 0
    }
  }

  /*
  P05 (*) Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  "Reverse a list" in {
    val result = S99.reverse(List(1, 1, 2, 3, 5, 8))
    result mustEqual List(8, 5, 3, 2, 1, 1)
  }
  "Reverse a list recusively" in {
    val result = S99.reverseRecursive(List(1, 1, 2, 3, 5, 8))
    result mustEqual List(8, 5, 3, 2, 1, 1)
  }
  "Reverse a list functionally" in {
    val result = S99.reverseFunctional(List(1, 1, 2, 3, 5, 8))
    result mustEqual List(8, 5, 3, 2, 1, 1)
  }

  /*
  P06 (*) Find out whether a list is a palindrome.
    Example:
    scala> isPalindrome(List(1, 2, 3, 2, 1))
  res0: Boolean = true
  */
  "find out if the list is a palindrome" in {
    S99.isPalindrome(List(1, 2, 3, 2, 1)) mustEqual true
    S99.isPalindrome(List(1, 2, 3, 3, 2, 1)) mustEqual true
    S99.isPalindrome(List(1, 2, 3, 2, 9)) mustEqual false
  }
  "find out if the list is a palindrome some other way" in {
    S99.isPalindromeQue(List(1, 2, 3, 2, 1)) mustEqual true
    S99.isPalindromeQue(List(1, 2, 3, 3, 2, 1)) mustEqual true
    S99.isPalindromeQue(List(1, 2, 3, 2, 9)) mustEqual false
  }

  /*
  P07 (**) Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  */
  "flatten nested lists" in {
    S99.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) mustEqual List(1, 1, 2, 3, 5, 8)
  }

  /*
  P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

   */

  "Eliminate consecutive duplicates of list elements" in {
    S99.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) mustEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }
  "Eliminate consecutive duplicates of list elements functionally" in {
    S99.compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) mustEqual List('a, 'b, 'c, 'a, 'd, 'e)
  }

  /*
  P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  */
  "Pack consecutive duplicates of list elements into sublists" in {
    val result = S99.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    result mustEqual List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  }

  /*
  P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:

scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  */
  "Run-length encoding of a list" in {
    val result = S99.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    result mustEqual List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }

  /*
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)) */
  "Modified run-length encoding" in {
    val result = S99.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    result mustEqual List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
  }

  /*
  * P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:

scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  "Decode a run-length encoded list" in {
    val result = S99.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    result mustEqual List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }
  */
}
