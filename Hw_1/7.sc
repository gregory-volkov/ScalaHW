def ListLength(InputList1: List[Int]): Int = {
  var Length = 0
  var InputList = InputList1
  while (!InputList.isEmpty) {
      InputList = InputList.tail
      Length+= 1
    }
  Length
}

