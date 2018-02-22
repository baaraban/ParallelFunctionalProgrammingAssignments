import scala.annotation.tailrec

type Set = Int => Boolean

def WholeNumbers: Set = (_) => true
def WholePositiveNumbers: Set = (x) => x >= 0

def contains ( s : Set , elem : Int ) : Boolean = s(elem)

def singletonSet(elem: Int): Set = (x) => x == elem

def union(s: Set, t: Set) : Set = (x) => s(x) || t(x)
def intersect(s: Set, t: Set): Set = (x) => s(x) && t(x)
def diff(s: Set, t: Set): Set = (x) => !(s(x) && t(x))


def filter ( s : Set , p : Int => Boolean ) : Set = intersect(s, p)

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

//def existsNotGood ( s : Set , p : Int => Boolean ) : Boolean = {
//  @tailrec
//  def iter ( a : Int ) : Boolean = {
//    if (a > 1000) false
//    else if (p(a)) true
//    else iter(a + 1)
//  }
//  iter(-1000)
//}

def map( s : Set , f : Int => Int ) : Set = x => s(f(x))

val one = singletonSet(1)
val two = singletonSet(2)
val three = singletonSet(3)
val oneTwoUnion = union(one, two)
val twoThreeUnion = union(two, three)
val shouldBeTwo = intersect(oneTwoUnion, twoThreeUnion)
val shouldBeOneThree = diff(oneTwoUnion, twoThreeUnion)
val shouldBeFilteredTwo = filter(oneTwoUnion, x => x % 2 == 0)
map(WholePositiveNumbers, x => -x)(-5)
exists(WholePositiveNumbers, x => x == 0)
forall(WholePositiveNumbers, x => x > 0)
forall(shouldBeFilteredTwo, x => x == 2)



