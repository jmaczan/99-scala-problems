/*
P01 (*) Find the last element of a list.
    Example:

    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
*/
def p01[T](list: List[T]): T = list.last

// without using a built-in function
def p01_1[T](list: List[T]): T = list.length match {
  case 1 => list.head
  case _ => p01_1(list.slice(1, list.length))
}

/*
P02 (*) Find the last but one element of a list.
Example:
  
  scala> penultimate(List(1, 1, 2, 3, 5, 8))
  res0: Int = 5
*/
 
def p02[T](list: List[T]): T = list(list.length - 2)
def p02_1[T](list: List[T]): Option[T] = list.length match {
  case 0 => None
  case 1 => None
  case _ => Some(list(list.length - 2))
}

/*
P03 (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.

    Example:

    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
*/
def p03[T](index: Int, list: List[T]): T = list(index)

@annotation.tailrec
def p03_1[T](index: Int, list: List[T]): T = index match {
  case 0 => list.head
  case _ => p03_1(index - 1, list.slice(1, list.length))
}

/*
P04 (*) Find the number of elements of a list.
    Example:

    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
*/
def p04[T](list: List[T]): Int = list.length

def p04_1[T](list: List[T]): Int = p04_internal(0, list)

@annotation.tailrec
def p04_internal[T](listLength: Int, list: List[T]): Int = list.headOption match {
  case None => listLength
  case _ => p04_internal(listLength + 1, list.tail)
}

/*
P05 (*) Reverse a list.
    Example:

    scala> reverse(List(1, 1, 2, 3, 5, 8))
    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/
def p05[T](list: List[T]): List[T] = list.reverse

def p05_1[T](list: List[T]): List[T] = list.foldLeft(List()) {
  (reversedList: List[T], element: T) => element :: reversedList
}

/*
P06 (*) Find out whether a list is a palindrome.
    Example:

    scala> isPalindrome(List(1, 2, 3, 2, 1))
    res0: Boolean = true
*/
def p06[T](list: List[T]): Boolean = list.length % 2 != 0 && p06_internal(true, list)

def p06_internal[T](isPalindrome: Boolean, list: List[T]): Boolean = list.length match {
  case 0 | 1 => isPalindrome
  case _ => list(0) == list(list.length - 1) match {
    case true => p06_internal(isPalindrome && true, list.slice(1, list.length - 1))
    case false => p06_internal(false, List())
  }
}

/*
P07 (**) Flatten a nested list structure.
    Example:

    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
*/
def p07[T](list: List[T | List[T]]): List[T] = list.foldLeft(List()) {
  (flattenedList: List[T], element: T | List[T]) => element.isInstanceOf[List[T]] match {
    case true => flattenedList ::: p07(element.asInstanceOf[List[T]])
    case false => flattenedList :+ element.asInstanceOf[T]
  }
}

/*
P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

    Example:

    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
*/
def p08[T](list: List[T]): List[T] = list.foldLeft(List()) {
  (compressedList: List[T], element: T) => compressedList.length > 0 && compressedList.last == element match {
    case true => compressedList
    case false => compressedList :+ element
  }
}

/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.

    Example:

    scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
*/
def p09[T](list: List[T]): List[List[T]] = list.foldLeft(List()) {
  (packedLists: List[List[T]], element: T) => packedLists.length > 0 && packedLists.last.last == element match {
    case true => ((packedLists.last :+ element) :: packedLists.slice(0, packedLists.length - 1).reverse).reverse
    case false => (List(element) :: packedLists.reverse).reverse
  }
}

/*
P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

    Example:

    scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/
def p10[T](list: List[T]): List[(Int, T)] = list.foldLeft(List()) {
  (encodedList: List[(Int, T)], element: T) => encodedList.length > 0 && encodedList.last._2 == element match {
    case true => ((encodedList.last._1 + 1, element) :: encodedList.slice(0, encodedList.length - 1).reverse).reverse
    case false => ((1, element) :: encodedList.reverse).reverse
  }
}

/*
P11 (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

    Example:

    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/
def p11[T](list: List[T]): List[(Int, T) | T] = list.foldLeft(List()) {
  (encodedList: List[(Int, T) | T], element: T) => encodedList.length > 0 && 
    encodedList.last.isInstanceOf[(Int, T)] &&
    encodedList.last.asInstanceOf[(Int, T)]._2 == element match {
      case true => ((encodedList.last.asInstanceOf[(Int, T)]._1 + 1, element) :: encodedList.slice(0, encodedList.length - 1).reverse).reverse
      case false => encodedList.length > 0 match {
        case true => encodedList.last == element match {
          case true => ((2, element) :: encodedList.slice(0, encodedList.length - 1).reverse).reverse
          case false => (element :: encodedList.reverse).reverse
        }
        case false => (element :: encodedList.reverse).reverse
      }
  }
}
