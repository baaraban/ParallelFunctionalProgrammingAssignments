import scala.annotation.tailrec

def mapReduce
(map: Int => Int, combine: (Int, Int) => Int, unit: Int)
(a: Int, b: Int): Int = {
  if(a > b) unit
  else
    combine(map(a), mapReduce(map, combine, unit)(a+1, b))
}

def mapReduceTail
(map: Int => Int, combine: (Int, Int) => Int, unit: Int)
(a: Int, b: Int): Int = {
  @tailrec
  def iter(currentResult: Int, a: Int): Int = {
    if(a > b) currentResult
    else
      iter(combine(map(a), currentResult), a+1)
  }
  if(a > b) unit
  else
    iter(unit, a)
}


mapReduce(x => x, (x, y) => x * y, 1)(1, 5)
mapReduceTail(x => x, (x, y) => x * y, 1)(1, 5)