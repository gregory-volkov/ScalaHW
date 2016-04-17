def ListSumOfElements(InputList1: List[Int]): Int = {
  var Sum = 0
  var InputList = InputList1
  while (!InputList.isEmpty) {
    Sum+= InputList.head
    InputList = InputList.tail
  }
  Sum
}

