import scala.annotation.tailrec

def factorial(x: Int): Int = {
  @tailrec
  def factorialRecursive(currentAnswer: Int, counter: Int): Int = {
    if (counter > x) {
      currentAnswer
    } else {
      factorialRecursive(currentAnswer*counter, counter + 1)
    }
  }
  factorialRecursive(1, 1)
}


def ex(x: Int, precision: Double): Double = {
  def recursiveEx(curAnswer: Double, step: Int):Double = {
    val toAdd:Double = Math.pow(x, step) / factorial(step)
    if(toAdd < precision){
      curAnswer + toAdd
    } else {
      recursiveEx(curAnswer + toAdd, step + 1)
    }
  }
  recursiveEx(1, 1)
}

ex(2, 0.0001)

def hanoi(n: Int) : Int = {
  if(n == 1 || n == 0){
    n
  } else {
   1 + 2*hanoi(n-1)
  }
}

hanoi(9)

