import scala.util.control.Breaks.{break}
def CheckForBrackets(InputList: List[Char]): Boolean = {
  var Balance = 0
  var Flag = true
  var CurElement = 0
  val ListLength = InputList.length
  while (CurElement <= ListLength - 1) {
    if (InputList.apply(CurElement) == '(') Balance+= 1 else Balance+= -1
    if (Balance < 0) {
      Flag = false
      break
    }
    CurElement+= 1
  }
  if (Balance != 0) {
    false
  } else {
    Flag
  }
}
