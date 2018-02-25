import scala.annotation.tailrec

type Set = Int => Boolean

def WholeNumbers: Set = (_) => true
def WholePositiveNumbers: Set = (x) => x >= 0

def contains ( s : Set , elem : Int ) : Boolean = s(elem)

def singletonSet(elem: Int): Set = (x) => x == elem

def emptySet: Set = (_) => false

def union(s: Set, t: Set) : Set = (x) => contains(s, x) || contains(t, x)
def intersect(s: Set, t: Set): Set = (x) => contains(s, x) && contains(t, x)
def alternativeOr(s: Set, t: Set): Set = (x) => !(contains(s, x) && contains(t, x))
def diff(s: Set, t: Set): Set = (x) => contains(s, x) && contains(t, x)


def filter ( s : Set , p : Int => Boolean ) : Set = x => contains(s, x) && p(x)

def forall ( s : Set , p : Int => Boolean ) : Boolean = {
  @tailrec
  def iter ( a : Int ) : Boolean = {
    if (a > 1000) true
      else if (!p(a) && contains(s, a)) false
      else iter(a + 1)
  }
  iter(-1000)
}

def exists ( s: Set, p: Int => Boolean) : Boolean = {
  !forall(s, x => !p(x))
}

def map( s : Set , f : Int => Int ) : Set = x => exists(s, y => f(y) == x)

val one = singletonSet(1)
val two = singletonSet(2)
val three = singletonSet(3)
val five = singletonSet(5)


val oneTwoUnion = union(one, two)
val twoThreeUnion = union(two, three)


val oneThreeUnion = union(one, three)
val threeFiveUnion = union(three, five)

val result: Set = map(threeFiveUnion, (x)=>x*x)

contains(result, 25)


