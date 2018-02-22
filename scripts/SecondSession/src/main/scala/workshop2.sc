def sumOfFuncs(
                f : Double => Double,
                g : Double => Double) : Double => Double = {
  (x) => f(x) + g(x)
}

var newFunc = sumOfFuncs(Math.sqrt, (x) => x * x)
newFunc(1)
newFunc(4)

var newNewFunc = sumOfFuncs(newFunc, Math.sqrt)
newNewFunc(1)
newNewFunc(4)


def vectorGenerator(values: List[Double]): Int => Double = {
  (index: Int) => values(index)
}

var vector1 = vectorGenerator(List(0, 8, 7, 3, 1, 2))

vector1(3)

def derivativeOf(f: Double => Double): Double => Double = {
  val step = 0.000001
  val value: Double => Double = (x: Double) => (f(x + step ) - f(x))/(step)
  value
}

derivativeOf(x => x*x)(1)
