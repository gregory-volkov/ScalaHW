val bound = 1000
val EmptySet: Set = (x: Int) => false
type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)
def singletonSet(elem: Int): Set = (x: Int) => (x == elem)
def union(s: Set, t: Set): Set = (x: Int) => (contains(s, x) || contains(t, x))
def intersect(s: Set, t: Set): Set = (elem: Int) => (contains(s, elem) && contains(t, elem))
def diff(s: Set, t: Set): Set = (elem: Int) => (contains(s, elem) && (!contains(t,elem)))
def filter(s: Set, p: Int => Boolean): Set = (intersect(s,p))
def toString(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else
      if ((contains(s, a)) && (!p(a))) false
        else
      iter(a + 1)
  }
  (iter(-bound))
}

def exists(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) false
    else
      if ((contains(s, a)) && (p(a))) true
        else
      iter(a + 1)
  }
  iter(-bound)
}

def map(s: Set, f: Int => Int): Set = {
  var ResSet: Set = EmptySet
  for (i <- -bound to bound) {
    if (contains(s, i)) {
      ResSet = union(ResSet, singletonSet(i))
    }
  }
  ResSet
}
