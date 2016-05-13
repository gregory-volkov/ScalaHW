def ListReverse(InputList : List[Int]): List[Int] = {
  def Iter(InputList: List[Int], ResList: List[Int]): List[Int] = {
    if (InputList.isEmpty)
      ResList
        else
      Iter(InputList.tail, InputList.head :: ResList)
  }
  Iter(InputList, List())
}
