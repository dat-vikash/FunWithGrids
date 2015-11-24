



// helper companion for Grid type alias
object Grid {
  def apply(xSize: Int, ySize: Int): Array[Array[Int]] = Array.ofDim[Int](xSize,ySize)
}

object GridPath
{
  // type alias for 2 dimensional array
  type Grid[T] = Array[Array[T]]
  // type alias for Move typpe
  type Move = String
  val East :Move = "East"
  val West :Move = "West"
  val North :Move = "North"
  val South :Move = "South"


  def validate(grid: Grid[Int], moves: List[Move]) : Boolean =
  {
    // the end node x and y indexes 
    val END_X_INDEX = grid.length - 1
    val END_Y_INDEX = grid(0).length - 1

    // nested function for returning the next x,y index pair based on a valid move selection
    def move(currX: Int, currY: Int, direction: Move): Tuple2[Int,Int] = direction match {
       case East =>   (currX + 1, currY)
       case West =>   (currX - 1, currY)
       case North =>   (currX , currY - 1)
       case South =>   (currX , currY + 1)
    }

    // trackers for indexes
   var currX: Int = 0
   var currY: Int = 0
    // set the initial hop balance to the number of hops for the starting node
   var hopBalance = grid(0)(0)
    var balanceExceededFlag =   false

    // iterate through the moves \
    for (moveDirection <- moves) {
      // check move hopBalance
      hopBalance match {
          case allowed if allowed > 0 => {
            // do the move and update current index position
            val (updatedXindex,updatedYindex) = move(currX,currY, moveDirection)
            currX = updatedXindex
            currY = updatedYindex
            // update the the hop balance
            hopBalance = hopBalance - 1 + grid(currX)(currY)
           }
          case notAllowed if notAllowed <= 0  => balanceExceededFlag = true //update flag
       }
   }
    // determine results of run
    (balanceExceededFlag,currX,currY) match {
      case (true,_,_) => false
      case (false,END_X_INDEX,END_Y_INDEX) => true
      case (fase,_,_) => false
    }
  }
}
