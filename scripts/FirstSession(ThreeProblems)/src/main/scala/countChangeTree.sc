def countChange(money: Int, coins: List[Int]): Int = {
  var results: List[List[Int]] = List[List[Int]]();
  def iterative(money: Int, currentCoins: List[Int]): Int = {
    if(money < 0){
      0
    } else if(money == 0){
      if(!results.exists(_.sorted == currentCoins.sorted)){
        results = results :+ currentCoins
        1
      } else {
        0
      }
    } else {
      var result: Int = 0;
      for(x <- coins ) result += iterative(money - x, currentCoins:+x)
      result
    }
  }

  val answer = iterative(money, List[Int]())
  for(l <- results){println(l.toString())}
  answer
}

def countChangeTrueTree(money: Int, coins: List[Int]): Long = {
  if(money < 0 || coins.isEmpty){
    0
  } else if(money == 0){
     1
  } else {
    countChangeTrueTree(money - coins.head, coins) + countChangeTrueTree(money, coins.tail)
  }
}

var s = countChange(49, List(2, 5, 10, 20))
var x = countChangeTrueTree(10000, List(1, 3, 7))
