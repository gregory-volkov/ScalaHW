def Combinations(n: Int, k: Int): Int = {
  def fact(n: Int): Int = {
    if (n == 0) 1 else n * fact(n-1)
  }
  val Num = {
    var Res = 1
    var i = 0
    for (i <- (k+1) to n) {
      Res*= i
    }
    Res
  }
  val Den = fact(n - k)
  Num / Den
}
