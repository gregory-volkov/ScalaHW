
val ListOfCoffs = List(1,2,3,4)
import math.pow

def CoubSolution(ListOfCoffs: List[Int]): Int = {
    val a = ListOfCoffs.apply(0)
    val b = ListOfCoffs.apply(1)
    val c = ListOfCoffs.apply(2)
    val d = ListOfCoffs.apply(3)
    val p = (3*a*c - b*b*b)/(3.0*a*a)
    val q = (2*pow(b,3) - 9*a*b*c + 27*a*a*d)/(27*pow(a,3))
    val Q = pow(p/3.0, 3) + pow(q/2.0, 3)
    
}