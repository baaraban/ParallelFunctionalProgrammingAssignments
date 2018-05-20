package TextScanner
import StringFunctions._

object CountStructure {
  def getCountPart(c: Char): CountStructure = if(isOrdinary(c)) WordPartStub(c.toString) else SeparationStub(c.toString)
}

sealed trait CountStructure {
  def getAmountOfWords(): Int = this match {
    case CountPart(_, x, _) => x
    case _ => 0
  }
}

case class EmptyStub() extends CountStructure

sealed abstract class Stub extends CountStructure
case class WordPartStub(s: String) extends Stub
case class SeparationStub(s: String) extends Stub

case class CountPart(leftStub: Stub, amountOfWords: Int, rightStub: Stub) extends CountStructure
