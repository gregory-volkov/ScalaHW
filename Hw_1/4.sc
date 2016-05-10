val ListOfCoffs: List[Double] = List(4,-3,1,-5) //List must always consist of 4 real numbers
                                                //In the case of the quadratic equation the zero element is zero

import math.{round,log, pow, sin, cos, exp, acos, Pi}
class Complex(a: Double, b: Double) {
  var Re = a
  var Im = b
  def toDouble = Re
  def isDouble = if (Im == 0) 1 else 0
  override def toString = {
    if (Im > 0) Re.toString + " + " + Im.toString + "i"
    else
    if (Im == 0) Re.toString
    else
      Re.toString + " - " + (-Im).toString + "i"
  }
  def -(that: Double) = new Complex(Re - that, Im)
  def +(that: Double) = new Complex(Re + that, Im)

}

def QuadOrCubEq(InputListOfCoffs: List[Double]): List[Any] = {

  def QuadSolution(InputListOfCoffs: List[Double]): List[Any] = {
    def abs(x: Double) = if (x > 0) x else -x
    val a = InputListOfCoffs.apply(1)
    val b = InputListOfCoffs.apply(2)
    val c = InputListOfCoffs.apply(3)
    val D = b * b - 4 * a * c
    if (D >= 0) {
      List((-b + pow(D, 1 / 2.0)) / (2 * a), (-b - pow(D, 1 / 2.0)) / (2 * a))
    } else {
      List(new Complex(0, (pow(abs(D), 1 / 2.0)) / (2.0 * a)) + (-b / (2.0 * a)), new Complex(0, -(pow(abs(D), 1 / 2.0)) / (2.0 * a)) + (-b / (2.0 * a)))
    }
  }

  def CubSolution(InputListOfCoffs: List[Double]): List[Any] = {
    def AddAtTheEnd[A](InputList: List[A], Element: A): List[A] = InputList ::: List(Element)
    def abs(x: Double) = if (x > 0) x else -x
    def sign(x: Double): Int = x match {
      case 0 => 0
      case _ => (x / abs(x)).toInt
    }
    def ch(x: Double) = (exp(x) + exp(-x)) / 2.0
    def sh(x: Double) = (exp(x) - exp(-x)) / 2.0
    def arch(x: Double) = log(x + pow(x * x - 1, 1 / 2.0))
    def ReduceEquation(InputList: List[Double]): List[Double] = {
      var i = 0
      var ResList: List[Double] = List(1)
      for (i <- 1 to InputList.length - 1) {
        ResList = AddAtTheEnd(ResList, InputList.apply(i) / InputList.apply(0))
      }
      ResList
    }

    val ListOfCoffs = ReduceEquation(InputListOfCoffs)
    val a = ListOfCoffs.apply(1)
    val b = ListOfCoffs.apply(2)
    val c = ListOfCoffs.apply(3)
    val Q = (a * a - 3 * b) / 9.0
    val R = (2 * pow(a, 3) - 9 * a * b + 27 * c) / 54
    val S = pow(Q, 3) - pow(R, 2)
    var EqSolution: List[Any] = List()
    if (S > 0) {
      val phi = 1 / 3.0 * acos(R / pow(Q, 3 / 2.0))
      EqSolution = List(-2 * pow(Q, 1 / 2.0) * cos(phi) - a / 3.0, -2 * pow(Q, 1 / 2.0) * cos(phi + 2 / 3.0 * Pi) - a / 3.0, -2 * pow(Q, 1 / 2.0) * cos(phi - 2 / 3.0 * Pi) - a / 3.0)
    }
    if (S < 0) {
      if (Q > 0) {
        val phi = 1 / 3.0 * arch(abs(R) / (pow(Q, 3 / 2.0)))
        EqSolution = List(-2 * sign(R) * pow(Q, 1 / 2.0) * ch(phi) - a / 3.0, new Complex(sign(R) * pow(Q, 1 / 2.0) * ch(phi) - a / 3.0, pow(3, 1 / 2.0) * pow(Q, 1 / 2.0) * sh(phi)), new Complex(sign(R) * pow(Q, 1 / 2.0) * ch(phi) - a / 3.0, -pow(3, 1 / 2.0) * pow(Q, 1 / 2.0) * sh(phi)))
      }
      if (Q < 0) {
        val phi = 1 / 3.0 * arch(abs(R) / (pow(abs(Q), 3 / 2.0)))
        EqSolution = List(-2 * sign(R) * pow(abs(Q), 1 / 2.0) * sh(phi) - a / 3.0, new Complex(sign(R) * pow(abs(Q), 1 / 2.0) * sh(phi) - a / 3.0, pow(3, 1 / 2.0) * pow(abs(Q), 1 / 2.0) * ch(phi)), new Complex(sign(R) * pow(abs(Q), 1 / 2.0) * sh(phi) - a / 3.0, -pow(3, 1 / 2.0) * pow(abs(Q), 1 / 2.0) * ch(phi)))
      }
      if (Q == 0) {
        val x1 = -pow(c - pow(a, 3) / 27.0, 1 / 3.0) - a / 3.0
        EqSolution = List(x1, new Complex(-(a + x1) / 2.0, 1 / 2.0 * pow(abs((a - 3 * x1) * (a + x1) - 4 * b), 1 / 2.0)), new Complex(-(a + x1) / 2.0, -1 / 2.0 * pow(abs((a - 3 * x1) * (a + x1) - 4 * b), 1 / 2.0)))
      }
    }
    if (S == 0) {
      EqSolution = List(-2 * pow(R, 1 / 3.0) - a / 3.0, pow(R, 1 / 3.0) - a / 3.0)
    }

    EqSolution
  }
  if (InputListOfCoffs.apply(0) == 0) {
    QuadSolution(InputListOfCoffs)
  } else {
    CubSolution(InputListOfCoffs)
  }
}

QuadOrCubEq(ListOfCoffs)
