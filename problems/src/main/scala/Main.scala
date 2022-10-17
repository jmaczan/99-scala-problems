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

  println("\nP05")
  println(p05(p01_list))
  println(p05_1(p01_list))

  println("\nP06")
  val palindrome = List(1, 2, 3, 2, 1)
  val notAPalindrome = List(1, 2, 3, 2, 1, 1)
  val notAPalindromeAsWell = List(1, 2, 3, 2, 1, 1, 2)
  println(p06(palindrome))
  println(p06(notAPalindrome))
  println(p06(notAPalindromeAsWell))

  println("\nP07")
  val nestedList = List(List(1, 1), 2, List(3, List(5, 8)))
  println(p07(nestedList))

  println("\nP08")
  val uncompressedList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  println(p08(uncompressedList))
}
