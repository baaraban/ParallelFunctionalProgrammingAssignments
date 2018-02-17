import scala.annotation.tailrec

def pascal(c: Long, r: Long): Long = {
    if(c < 0 || r < 0 || c > r){
        -1
    } else if(c == 0 || r == c)
        1
    else
        pascal(c-1, r - 1) + pascal(c, r - 1)
}


def pascalTailRec(c: Long, r: Int): Long = {
    def walkThrough(calcTriangle: List[(Long, Long, Long)],
                    curCol:Long,
                    curRow: Long): Long = {

        def aboveLeft: Long ={
            if(curCol < 0 || curRow < 0) {

                1
            }
            else {
                println(curCol, curRow)
                println(calcTriangle)
                calcTriangle.filter(x => (x._1 == curCol - 1)
                  && (x._2 == curRow - 1)).head._3
            }
        }

        def above: Long = {
            if(curRow < 0 || curCol < 0)
                1
            else
                calcTriangle.filter(x => (x._1 == curCol)
                    && (x._2 == curRow - 1)).head._3
        }
        def calculateElement: Long = {
            above*aboveLeft
        }
        if(curCol == c && curRow == r){
            calculateElement
        } else {
            var newCol = curCol + 1
            var newRow: Long = 0
            if(newCol > curRow){
                newCol = 0
                newRow = curRow + 1
            } else {
                newRow = curRow
            }
            println(curCol, curRow)
            walkThrough(calcTriangle :+ (curCol, curRow, calculateElement),
                newCol, newRow)
        }
    }
    if(c < 0 || r < 0 || c > r){
        -1
    } else if(c == 0 || r == c)
        1
    else
        walkThrough(List((0,0,1)), 0, 1);
}

pascalTailRec(1, 2)
