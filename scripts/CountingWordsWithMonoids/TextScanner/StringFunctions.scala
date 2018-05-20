package TextScanner

object StringFunctions {
  val emptyString = ""
  val ordinary = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet

  def isOrdinary(c: Char)= ordinary.contains(c)
  def isSpecialCharacter(c: Char) = !ordinary.contains(c)
}
