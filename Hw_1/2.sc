def CheckForBrackets(InputList: List[Char]): Int = {
  var Balance = 0
  var Flag = 1
  var CurElement = 0
  val ListLength = InputList.length
  while (CurElement <= ListLength - 1) {
    if (InputList.apply(CurElement) == '(') Balance+= 1 else Balance+= -1
    if (Balance < 0) Flag = 0
    CurElement+= 1
  }
  if (Balance != 0) {
    0
  } else {
    Flag
  }
}

