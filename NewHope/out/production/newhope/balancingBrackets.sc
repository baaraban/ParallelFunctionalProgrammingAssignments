import scala.annotation.tailrec

def isBalanced(chars: List[Char]): Boolean = {
  def recurProcess(curOpened: Int, chars: List[Char]): Boolean = {
    if (chars.isEmpty) {
      curOpened == 0
    }
    else if (chars.head == '('){
      recurProcess(curOpened + 1, chars.tail)
    } else if (chars.head == ')') {
      recurProcess(curOpened - 1, chars.tail)
    } else recurProcess(curOpened, chars.tail)
  }
  recurProcess(0, chars)
}

isBalanced("!((1+(some)))".toList)


