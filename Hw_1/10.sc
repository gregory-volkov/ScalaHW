def Predicate(x: Int): Boolean = if (x % 2 == 1) true else false
def FilterOfList(InputList: List[Int], Predicate: Int => Boolean): List[Int] = {
  def Iter(InputList: List[Int]): List[Int] = {
    if (InputList.isEmpty)
      List()
    else
      if (Predicate(InputList.head))
      InputList.head :: Iter(InputList.tail)
      else
        Iter(InputList.tail)
  }
  Iter(InputList)
}
