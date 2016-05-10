def Predicate(x: Int): Int = x % 2
def FilterOfList(InputList: List[Int], Predicate:(Int) => Int) = {
  def AddAtTheEnd[A](InputList: List[A], Element: A): List[A] = InputList ::: List(Element)
  var ResList: List[Int] = List()
  for (i <- 0 to InputList.length - 1) {
    if (InputList.apply(i) % 2 == 1) ResList = AddAtTheEnd(ResList, InputList.apply(i))
  }
  ResList
}
