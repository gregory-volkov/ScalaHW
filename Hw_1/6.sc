def AddAtTheEnd[A](InputList: List[A], Element: A): List[A] = {
  if (InputList.isEmpty)
    List(Element)
  else
    InputList.head :: AddAtTheEnd(InputList.tail, Element)
}
