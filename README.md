To run use SBT console.

scala'''

import GridPath._

val grid: Grid[Int] = Grid(3,3)
grid(0)(0) = 1
grid(0)(1) = 2
grid(0)(2) = 0
grid(1)(2) = 1

val moves: List[Move] = List(South, South, East, East)

validate(grid,moves)


findAllPaths(grid)

'''
