def ListLength[A](InputList: List[A]): Int = {
  def Iter[A](InputList: List[A], Acc: Int): Int = {
    if (!InputList.isEmpty)
      Iter(InputList.tail, Acc + 1)
    else
      Acc  
  }
  Iter(InputList, 0)
}
