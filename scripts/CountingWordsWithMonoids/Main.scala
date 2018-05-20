import TextScanner.{CountStructure, EmptyStub, TextMonoid}

import scala.io.Source

object Main {
  val filePath = "some.txt"
  val HYPER_LINE_THRESHOLD = 1000

  def main(args: Array[String] ): Unit = {
    var hyperLine = ""
    var emptyLines = 0
    var currentRes: CountStructure = EmptyStub()

    val bufferedSource = Source.fromFile(filePath)
    for(line <- bufferedSource.getLines()){
      if(!line.isEmpty())
        if(hyperLine.length + line.length > HYPER_LINE_THRESHOLD){
          currentRes = TextMonoid.textMonoid.combine(currentRes, TextScanner.Scanner.countWordsPar(hyperLine))
          hyperLine = ""
        } else {
          hyperLine += " " + line
        }
      else
        emptyLines += 1
    }
    bufferedSource.close()
    hyperLine += "_"
    println(TextMonoid.textMonoid.combine(currentRes, TextScanner.Scanner.countWordsPar(hyperLine)).getAmountOfWords())
  }
}
