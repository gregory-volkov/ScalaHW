def ListLength[A](InputList: List[A]): Int = {
  if (!InputList.isEmpty) 
    ListLength(InputList.tail) + 1 
  else 
    0
}
