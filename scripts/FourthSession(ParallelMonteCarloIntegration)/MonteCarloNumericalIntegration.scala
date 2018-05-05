import scala.util.Random
import org.scalameter._
import ua.edu.ucu.cs.parallel._

object MonteCarloNumericalIntegration {
  def integrateMonteCarloParInTwo(func: Double => Double)
                                 (interval: (Double, Double))
                                 (numberOfRandomPoints: Double = 100000): Double = {
    val middle = (interval._1 + interval._2) / 2
    val partitions = numberOfRandomPoints/2
    val (s1, s2) = parallel(integrateMonteCarlo(func)((interval._1, middle))(partitions),
      integrateMonteCarlo(func)((middle, interval._2))(partitions))
    s1 + s2
  }

  def integrateMonteCarloParInFour(func: Double => Double)
                                 (interval: (Double, Double))
                                 (numberOfRandomPoints: Double = 100000): Double = {
    val middle = (interval._1 + interval._2) / 2
    val partitions = numberOfRandomPoints / 2
    val (s1, s2) = parallel(integrateMonteCarloParInTwo(func)((interval._1, middle))(partitions),
      integrateMonteCarloParInTwo(func)((middle, interval._2))(partitions))
    s1 + s2
  }

  def integrateMonteCarloParInEight(func: Double => Double)
                                  (interval: (Double, Double))
                                  (numberOfRandomPoints: Double = 100000): Double = {
    val middle = (interval._1 + interval._2) / 2
    val partitions = numberOfRandomPoints /2
    val (s1, s2) = parallel(integrateMonteCarloParInFour(func)((interval._1, middle))(partitions),
      integrateMonteCarloParInFour(func)((middle, interval._2))(partitions))
    s1 + s2
  }

  def integrateMonteCarlo(func: Double => Double)
                         (interval: (Double, Double))
                         (numberOfRandomPoints: Double = 1000000): Double = {
    val randomX = new Random
    def iter(generated: Int, sum: Double): Double ={
      def randomRectAreaCalc(a: Double, b: Double) = func(randomX.nextDouble()*(b - a) + a)
      if(generated >= numberOfRandomPoints)
        sum
      else{
        iter(generated + 1, sum + randomRectAreaCalc(interval._1, interval._2))
      }
    }
    (interval._2 - interval._1) * iter(0, 0) / numberOfRandomPoints
  }

  def main(args: Array[String]): Unit = {

    val standardConfig = config(
      Key.exec.minWarmupRuns ->100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new Warmer.Default);


    val seqtime = standardConfig.measure{
      integrateMonteCarlo(x => math.exp(x))((2, 5))()
    }

    val partimeTwoThreads = standardConfig.measure{
      integrateMonteCarloParInTwo(x => math.exp(x))((2, 5))()
    }

    val partimeFourThreads = standardConfig.measure{
      integrateMonteCarloParInFour(x => math.exp(x))((2, 5))()
    }

    val partimeEightThreads = standardConfig.measure{
      integrateMonteCarloParInEight(x => math.exp(x))((2, 5))()
    }

    println(s"sequantional time: $seqtime ms")
    println(s"parallel time(2): $partimeTwoThreads ms")
    println(s"parallel time(4): $partimeFourThreads ms")
    println(s"parallel time(8): $partimeEightThreads ms")
    println(s"speedup(2): ${seqtime.value/partimeTwoThreads.value}")
    println(s"speedup(4): ${seqtime.value/partimeFourThreads.value}")
    println(s"speedup(8): ${seqtime.value/partimeEightThreads.value}")
    println(s"result: ${integrateMonteCarloParInEight(x => math.exp(x))((0, 1))()}")
  }
}
