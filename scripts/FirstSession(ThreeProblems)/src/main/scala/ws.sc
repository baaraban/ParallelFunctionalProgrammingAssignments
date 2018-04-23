  def square(value: Double) = value * value
  def avg(x: Double, y:Double) = (x + y)/ 2

  def abs(value: Double) = if (value > 0) value else -value

  def newtonSqrt(x: Double): Double = {

    def isGoodEnough(estimation: Double): Boolean = abs(square(estimation) - x) < 0.001

    def improveEstimation(estimation: Double): Double = avg(estimation, x / estimation)

    def sqrtIteration(estimation: Double, value: Double): Double = {
      if (isGoodEnough(estimation))
        estimation
      else
        sqrtIteration(improveEstimation(estimation), value)

    }

   sqrtIteration(1, x)

  }

  newtonSqrt(16)









