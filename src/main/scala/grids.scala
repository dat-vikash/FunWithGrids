



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

  // helper function for returning the next x,y index pair based on a valid move selection 
  def move(currX: Int, currY: Int, direction: Move): Tuple2[Int,Int] = direction match { 
   case East =>   (currX + 1, currY) 
   case West =>   (currX - 1, currY) 
   case North =>   (currX , currY - 1) 
   case South =>   (currX , currY + 1) 
   }

  // helper function to check if we are within grid bounds
  def boundsCheck(currX: Int, currY: Int, grid: Grid[Int]): Boolean = ( currX >= grid.length || currX < 0) || (currY >= grid(0).length || currY < 0)

  // function to validate a given move list
  def validate(grid: Grid[Int], moves: List[Move]) : Boolean =
  {
    // the end node x and y indexes 
    val END_X_INDEX = grid.length - 1
    val END_Y_INDEX = grid(0).length - 1

    // nested helper function to check if we are at the goal node
    def goalCheck(currX: Int, currY: Int, endX: Int, endY: Int): Boolean = currX == endX && currY == endY

    // trackers for indexes
   var currX: Int = 0
   var currY: Int = 0
    // set the initial hop balance to the number of hops for the starting node
   var hopBalance = grid(0)(0)
   var exception: Option[Exception] = None


    // iterate through the moves
    for (moveDirection <- moves) {
      // check move hopBalance
      hopBalance match {
          case allowed if allowed > 0 && !goalCheck(currX,currY,END_X_INDEX,END_Y_INDEX)  => {
            // do the move and update current index position
            val (updatedXindex,updatedYindex) = move(currX,currY, moveDirection)
            currX = updatedXindex
            currY = updatedYindex
            // update the the hop balance if next move was within bounds
            boundsCheck(currX,currY,grid) match {
              case false => hopBalance = hopBalance - 1 + grid(currX)(currY)
              case true => exception = Some(new IllegalStateException("out of grid bounds"))
            }
           }
          case allowed if allowed > 0 && goalCheck(currX,currY,END_X_INDEX,END_Y_INDEX)  => {
            exception =  Some(new IllegalStateException("final cell reached, but you still have moves"))
          }

          case notAllowed if notAllowed <= 0  => exception =Some(new IllegalStateException("out of hops")) //update flag
       }
   }
    // determine results of run
    (exception,currX,currY) match {
      case (Some(ex),_,_) => { println(ex.getMessage);false}
      case (None,END_X_INDEX,END_Y_INDEX) => true
      case (None,_,_) => {println("out of moves");false}
    }
  }

  // Recursive function that finds all possible solutions without revisiting already visited nodes
  def printAllPaths(current: Tuple2[Int,Int], end: Tuple2[Int,Int], balance: Int,
                    grid: Grid[Int], paths: List[Move], visited: List[Tuple2[Int,Int]]): Unit = { 

     // if the move puts us out of bound or goes to an already visited node, lets return Unit 
    if(boundsCheck(current._1,current._2,grid) || visited.exists(p => p==current)) {
      ()
    }
    // if hop balance is less than or equal to 0, we have run out of moves 
    else if (balance <= 0) {
      ()
    }
    // if the current node is equal to the end node, we have found a path 
    else if(current == end) {
      print("Solution: ") 
      paths.foreach( x=> print(s"$x ")) 
      println("") 
    }
    // intiante rescursive search with orthogonal movements 
    else { 
      val updatedVisits = visited :+ current 
      // get all adjacent entries 
      val east = move(current._1,current._2,East) 
      val west = move(current._1,current._2,West) 
      val north = move(current._1,current._2,North) 
      val south = move(current._1,current._2,South) 
      val newbalance = balance - 1 + grid(current._1)(current._2) 
      printAllPaths(east,end,newbalance,grid, paths :+ East,updatedVisits) 
      printAllPaths(west,end,newbalance,grid, paths :+ West,updatedVisits) 
      printAllPaths(north,end,newbalance,grid, paths :+ North,updatedVisits) 
      printAllPaths(south,end,newbalance,grid, paths :+ South,updatedVisits) 
    } 
  } 

  // Function that drives a depth first search for possible solutions 
  def findAllPaths(grid:Grid[Int]) =   {
     val paths: List[Move] = List.empty 
    val balance: Int = grid(0)(0) 
    val visited: List[Tuple2[Int,Int]] = List.empty 
    printAllPaths(Tuple2(0,0),Tuple2(grid.length - 1, grid(0).length - 1), balance, grid, paths, visited)  
  }
}
