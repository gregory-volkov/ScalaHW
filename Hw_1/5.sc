def ListReverse(InputList : List[Int]): List[Int] = {
  var ResList: List[Int] = List()
  val i = 0
  def Iter(ResList: List[Int], NewElement: Int): List[Int] = {
      val TempList = List(NewElement)
      TempList ::: ResList
    }
  for (i <- 0 to InputList.length - 1) {
    ResList = Iter(ResList, InputList.apply(i))
  }
  ResList
}
