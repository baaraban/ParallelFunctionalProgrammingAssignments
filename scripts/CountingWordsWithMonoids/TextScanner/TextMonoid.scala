package TextScanner



object TextMonoid {

  trait Monoid[A] {
    def combine(x: A, y: A): A
    def neutral: A
  }

  def textMonoid = new Monoid[CountStructure]{
    override def combine(x: CountStructure, y: CountStructure): CountStructure = (x, y) match {
      case (EmptyStub(), _) => y
      case (x, EmptyStub()) => x
      case (SeparationStub(l), SeparationStub(r)) => SeparationStub(l + r)
      case (WordPartStub(l), WordPartStub(r)) => WordPartStub(l + r)

      case (SeparationStub(s), WordPartStub(w)) => CountPart(SeparationStub(s), 0,  WordPartStub(w))
      case (WordPartStub(w), SeparationStub(s)) => CountPart(WordPartStub(w), 0,  SeparationStub(s))

      case (CountPart(l, x, SeparationStub(s1)), SeparationStub(s2)) => CountPart(l, x, SeparationStub(s1 + s2))
      case (CountPart(l, x, SeparationStub(_)), WordPartStub(w)) => CountPart(l, x, WordPartStub(w))
      case (CountPart(l, x, WordPartStub(_)), SeparationStub(s)) => CountPart(l, x + 1, SeparationStub(s))
      case (CountPart(l, x, WordPartStub(w1)), WordPartStub(w2)) => CountPart(l, x, WordPartStub(w1 + w2))

      case (SeparationStub(s1), CountPart(SeparationStub(s2), x, r)) => CountPart(SeparationStub(s1 + s2), x, r)
      case (WordPartStub(w), CountPart(SeparationStub(_), x, r)) => CountPart(WordPartStub(w), x, r)
      case (SeparationStub(s), CountPart(WordPartStub(_), x, r)) => CountPart(SeparationStub(s), x + 1, r)
      case (WordPartStub(w1), CountPart(WordPartStub(w2), x, r)) => CountPart(WordPartStub(w1 + w2), x, r)

      case (CountPart(l, x1, m1), CountPart(m2, x2, r)) =>{
        val middle = combine(m1, m2)
        val additional = middle match {
          case CountPart(_, _, _) => 1
          case WordPartStub(_) => 1
          case _ => 0
        }
        CountPart(l, x1 + x2 + additional, r)
      }
    }

    override def neutral: CountStructure = EmptyStub()
  }
}
