def CountChange(Money: Int, Coins: List[Int]): Int = {
  var NumOfWays = new Array[Int](Money + 1)
  val CurMoney = 0;
  val Coin = 0;
  for (CurMoney <- 0 to Money) {
    for (Coin<- Coins) {
      if (CurMoney + Coin <= Money) NumOfWays(CurMoney + Coin)+= 1
    }
    }
  for (Coin <- 0 to Money){
    println(NumOfWays(Coin))
  }

  NumOfWays(Money)
}

CountChange(6, List(1,2,3))
