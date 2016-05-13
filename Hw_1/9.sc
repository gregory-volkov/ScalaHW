def FilterOfList(InputList: List[Int]): List[Int] = {
  def Iter(InputList: List[Int]): List[Int] = {
    if (InputList.isEmpty)
      List()
    else
      if (InputList.head % 2 == 0)
      InputList.head :: Iter(InputList.tail)
      else
        Iter(InputList.tail)
  }
  Iter(InputList)
}
