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
}
