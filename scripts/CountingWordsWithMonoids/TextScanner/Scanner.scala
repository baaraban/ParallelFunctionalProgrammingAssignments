package TextScanner

import Foldings._
import TextMonoid._

object Scanner {
  implicit val thresholdSize = 30
  def mappingFunc(x: Char): CountStructure = {
    CountStructure.getCountPart(x)
  }

  def countWordsPar(str: String): CountStructure = {
    foldMapPar(str, 0, str.length, textMonoid)(mappingFunc)
  }
}
