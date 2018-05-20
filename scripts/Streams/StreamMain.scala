import Streams.StreamHomework._

object StreamMain {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3, 4, 5)
    println(stream.toList)
    val top3 = stream.drop(4)
    println(top3.exists( x => x == 3))
    println(stream.forAllThroughFoldRight(x => x % 2 == 0))

    val more = Stream.from(100)
    println(more.take(5).toList)

    val fibs = {
      def next(f0: Int, f1: Int): Stream[Int] =
        Stream.cons(f0, next(f1, f0 + f1))

      next(0, 1)
    }

    val prime = {
      def findNextPrime(current: Int): Int = {
        def isPrime(n: Int): Boolean = ! ((2 until n-1) exists (n % _ == 0))
        if(isPrime(current + 1))
          current + 1
        else
          findNextPrime(current + 1)
      }

      def next(cur: Int): Stream[Int] =
        Stream.cons(cur, next(findNextPrime(cur)))

      next(3)
    }

    println(fibs.take(20).toList)

    println(fibs.filter(x => x % 2 == 0).take(7).toList)
    println(fibs.map(x => x*x).take(5).toList)

    println(prime.take(15).toList)
  }

}
