def SumOfListElements(InputList: List[Int]): Int = {
  var Sum = 0
  if 
    (!InputList.isEmpty) InputList.head + SumOfListElements(InputList.tail) 
  else 
    0
}
