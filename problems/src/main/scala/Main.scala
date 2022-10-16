@main def problems: Unit = {
  println("P01")
  val p01_list = List(1, 1, 2, 3, 5, 8)
  println(p01(p01_list))
  println(p01_1(p01_list))

  println("\nP02")
  println(p02(p01_list))
  println(p02_1(p01_list) getOrElse "There is no elements enough to tell what element is last but one")
  println(p02_1(List(1)) getOrElse "There is no elements enough to tell what element is last but one")

  println("\nP03")
  println(p03(2, p01_list))
  println(p03_1(2, p01_list))

  println("\nP04")
  println(p04(p01_list))
  println(p04_1(p01_list))
}

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