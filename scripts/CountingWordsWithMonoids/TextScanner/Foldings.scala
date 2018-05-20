package TextScanner

import TextMonoid._
import parallel.parallel

object Foldings {
  def foldMapSegment[A, B](xs: IndexedSeq[A], from: Int, to: Int, m: Monoid[B])
                          (f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while(index < to){
      res = m.combine(res, f(xs(index)))
      index += 1
    }
    res
  }

  def foldMapPar[A, B](xs: IndexedSeq[A], from: Int, to: Int, m: Monoid[B])
                      (f: A => B)
                      (implicit thresholdSize: Int): B = {
    if(to - from < thresholdSize){
      foldMapSegment(xs, from, to, m)(f)
    } else {
      val middle = from + (to - from)/2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)(thresholdSize),
        foldMapPar(xs, middle, to, m)(f)(thresholdSize)
      )
      m.combine(l, r)
    }
  }

  def foldMap[A, B](xs: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    xs.foldLeft(m.neutral)((b, a) => m.combine(b, f(a)))
  }

}
