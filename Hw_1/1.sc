def CheckForBrackets(InputList: List[Char]): Int = {
  var Balance = 0
  var Flag = 1
  var CurElement = 0
  val ListLength = InputList.length
  while (CurElement <= ListLength) {
    if (InputList.apply(CurElement) == '(') Flag+= 1 else Flag+= -1
    if (Balance < 0) Flag = 0
    CurElement+= 1
  }
  Flag
}

CheckForBrackets(InputList)
