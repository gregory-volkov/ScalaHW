def Predicate(x: Int): Int = x % 2

def AddAtTheEnd(InputList: List[Int], Element: Int): List[Int] = {
  var TempList = List(Element)
  InputList ::: TempList
}

def FilterOfList(InputList1: List[Int], Predicate:(Int) => Int) = {
  var InputList = InputList1
  var ResList: List[Int] = List()
  while (!InputList.isEmpty) {
    if (Predicate(InputList.head) == 1) ResList = AddAtTheEnd(ResList, InputList.head)
    InputList = InputList.tail
  }
  ResList
}

