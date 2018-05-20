package Streams

object StreamHomework {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }

    def toList: List[A] = {
      @annotation.tailrec
      def move(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => move(t(), h() :: acc)
        case _ => acc
      }

      move(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match{
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match{
      case _ if n < 1 => this
      case Cons(_, t) if n > 1 => t().drop(n - 1)
      case Cons(_, t) if n == 1 => t()
      case _ => Stream.empty
    }

    def exists(p: A => Boolean): Boolean = this match{
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def existsThroughFoldRight(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def foldRight[B](z: => B)(f: (A, => B) => B):B = this match{
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean = this match{
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }

    def forAllThroughFoldRight(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def filter(f: A => Boolean): Stream[A] = this match{
      case Cons(h, t) if f(h()) => Stream.cons(h(), t().filter(f))
      case Cons(_, t) => t().filter(f)
      case _ => this
    }

    def map[B](f: A => B): Stream[B] = this match{
      case Cons(h, t) => Stream.cons(f(h()), t().map(f))
      case _ => Stream.empty[B]
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](args: A*): Stream[A] = {
      if (args.isEmpty) empty
      else cons(args.head, apply(args.tail: _*))
    }

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))
  }

}