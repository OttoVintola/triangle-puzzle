package trianglepuzzle

import scala.util.Random
import scala.collection.mutable.Buffer
import o1.GridPos

object Cheat:

  // all of the trianlges that are matcing so far
  // this is a val since it is a buffer
  // it could be a var, but then it would have to be a vector
  // and also I would have to change the program to update the collection differently
  var solutionSet: Buffer[realTriangle] = Buffer[realTriangle]()


  // all of the positions that are still unoccupied
  // Using val and Vector since it is completely static
  // At no point do the individual values change or the length of the vector
  val freePositions: Vector[GridPos] = Puzzle.board.allPositions.filterNot(
      pos => Puzzle.board.elementAt(pos).isPlaced ||
        (pos.x == 6 && pos.y == 3) ||
        (pos.x == 0 && pos.y == 3) ||
        (pos.x == 0 && pos.y == 0) ||
        (pos.x == 6 && pos.y == 0)).toVector


   // Helper method to find all of the fitting triangles
   // using .filter would be more complicated than it looks
   // just doing triangles.filter(triangle => Puzzle.board.matches(triangle.coordinates)) would not work
   // Since the triangle has to be placed on the board in the position first before it can be checked if it matches with its neighbors
   // and since only one triangle can be placed at a time, it has to be checked one by one
   def fittingTriangles(triangles: Buffer[realTriangle], position: GridPos): Buffer[realTriangle] =
    val fitting = Buffer[realTriangle]()
    for triangle <- triangles do
        Puzzle.board.update(position, triangle)
        if Puzzle.board.matches(position) then
          fitting.append(triangle)
          Puzzle.board.update(position, realTriangle(false, "fff"))
    Random.shuffle(fitting).toBuffer


   // Just gets the previous position from the list of free positions
   def getPreviousPosition(positions: Buffer[GridPos], current: GridPos): GridPos =
    val index = freePositions.indexOf(current)
    if index == 0 then
      GridPos(1, 0)
    else
      freePositions(index - 1)


   // Attempt number 1
   /*def solve(triangles: Buffer[realTriangle], poses: Buffer[GridPos]): Boolean =
       var positions = poses
       var unusedTriangles = triangles
       var solutionFound = false

       while (unusedTriangles.nonEmpty && !solutionFound) do
           var position = positions.head
           var fitting = fittingTriangles(unusedTriangles, position)

           while (fitting.isEmpty && positions.length > 1) do
               // backtrack to the previous position until a new fitting triangle is found
               val previous = getPreviousPosition(positions, position)
               Puzzle.board.update(position, realTriangle(false, "fff"))
               unusedTriangles.append(setSoFar.last)
               setSoFar.remove(setSoFar.length - 1)
               fitting = fittingTriangles(unusedTriangles, previous)
               positions.prepend(previous)
               position = previous


           if fitting.isEmpty then
               // if there is no fitting triangle at the previous position, the puzzle is unsolvable
               solutionFound = false
           else
               // place the fitting triangle at the current position
               val triangle = fitting.head
               if !triangle.triedPositions.contains(position) then
                  triangle.triedPositions.append(position)
                  unusedTriangles = unusedTriangles.filterNot(_ == triangle)
                  Puzzle.board.update(position, triangle)
                  triangle.coordinates = position
                  triangle.isPlaced = true
                  Cheat.setSoFar.append(triangle)
                  positions -= position


                  if positions.isEmpty then
                      // if there are no more positions to fill, the puzzle is solved
                      solutionFound = true
                  else
                      // recursively solve the puzzle for the remaining positions and triangles
                      solutionFound = solve(unusedTriangles, positions)
                      if !solutionFound then
                          // backtrack and try the next unused triangle
                          Puzzle.board.update(position, realTriangle(false, "fff"))
                          triangle.isPlaced = false
                          unusedTriangles.append(triangle)
                          Cheat.setSoFar.remove(Cheat.setSoFar.length - 1)
                          positions = positions.prepend(position)
               else
                  unusedTriangles = unusedTriangles.filterNot(_ == triangle)


       solutionFound*/


  // Attempt number 2
  /*
  def solve(triangles: Buffer[realTriangle], poses: Buffer[GridPos]): Boolean =
      var unusedTriangles = triangles
      var freePositions = poses.filterNot(pos => Puzzle.board.elementAt(pos).isPlaced)
      var setSoFar: Buffer[realTriangle] = Buffer[realTriangle]()
      var triedPositions = Set[GridPos]()
      var solutionFound = false

      while (unusedTriangles.nonEmpty && !solutionFound)
        val position = freePositions.find(pos => !triedPositions.contains(pos)).getOrElse(freePositions.head)
        triedPositions += position
        val fitting = fittingTriangles(unusedTriangles, position)

        if (fitting.nonEmpty) then
          val triangle = Random.shuffle(fitting).head
          triangle.coordinates = position
          Puzzle.board.update(position, triangle)
          setSoFar.append(triangle)
          unusedTriangles = unusedTriangles.filterNot(_ == triangle)

          if (freePositions.tail.isEmpty) then
            solutionFound = true
          else
            freePositions = freePositions.filterNot(_ == position)

        else
          if (setSoFar.nonEmpty) then
            val lastTriangle = setSoFar.last
            val lastPosition = lastTriangle.coordinates
            Puzzle.board.update(lastPosition, realTriangle(false, "fff"))
            setSoFar = setSoFar.dropRight(1)
            unusedTriangles.append(lastTriangle)
            freePositions = freePositions.filterNot(_ == position)
            freePositions = freePositions.prepend(lastPosition)
           else
            solutionFound = false

      solutionFound
    }*/

   // nothing works
   // ever
   // every solution leads to a bigger issue


   // some use recursion
   // all of them use backtracking
   // nothing works
   // is it backtracking?
   // is it recursion?
   // is it a helper method?
   // if so which one
   // onwards




   // Attempt number 3
   /*def solve(triangles: Buffer[realTriangle], poses: Buffer[GridPos]): Boolean =
     var unusedTriangles = triangles
     var solutionFound = false
     var positionIndex = 0



     while (unusedTriangles.nonEmpty && !solutionFound) do
        var position = freePositions(positionIndex)
        val fitting = fittingTriangles(unusedTriangles, position)


        if fitting.nonEmpty then
          val triangle = fitting.head
          triangle.coordinates = position
          Puzzle.board.update(position, triangle)
          setSoFar.append(triangle)
          unusedTriangles = unusedTriangles.filterNot(_ == triangle)
          positionIndex += 1


          if positionIndex == freePositions.length then
            solutionFound = true

        else
          if setSoFar.nonEmpty then
            val lastTriangle = setSoFar.last
            val currentIndex = freePositions.indexOf(position)
            val previousPosition = if currentIndex != 0 then freePositions(currentIndex - 1) else freePositions.head
            Puzzle.board.update(previousPosition, realTriangle(false, "fff"))
            setSoFar.dropRight(1)
            unusedTriangles.append(lastTriangle)
            positionIndex -= 1
            position = freePositions(positionIndex)
          else
            solutionFound = false


     solutionFound*/


    // yet another attempt...
    // in the endless search for the elusive algorithm


   // IT WORKS!!!
   def solve(triangles: Buffer[realTriangle], positions: Buffer[GridPos]): Boolean =
      var unusedTriangles = triangles
      var places = positions
      var solutionFound = false
      var position = places.head



      def backtrack(currentPosition: GridPos): Unit =
        // Cannot backtrack behind the first position
        if currentPosition != freePositions.head then
          val currentIndex = freePositions.indexOf(currentPosition)
          val previousPosition = if currentIndex == 0 then freePositions.head else freePositions(currentIndex-1)
          val previousTriangle = Puzzle.board.elementAt(previousPosition)
          // When the program backtracks it needs to add the backtracked position to the list of positions
          places.prepend(previousPosition)
          // place an empty triangle in the position that was backtracked from
          Puzzle.board.update(currentPosition, realTriangle(false, "fff"))
          // add the backtracked triangle to the list of unused triangles
          unusedTriangles.append(previousTriangle)
          // remove the backtracked triangle from the list solution triangles
          solutionSet = solutionSet.dropRight(1)
          println(s"backtracking from $currentPosition to $previousPosition")


      // this is a pretty straightforward method
      // just places a triangle at the position that is defined above
      def place(triangle: realTriangle): Unit =
        Puzzle.board.update(position, triangle)
        // remove the position that was just used from the list of free positions
        places = places.drop(1)
        // remove the triangle that was just placed from the list of unused triangles
        unusedTriangles = unusedTriangles.filterNot(_ == triangle)
        // add the triangle that was just placed to the list of solution triangles
        solutionSet.append(triangle)
        // set the previous placement to the current placement
        triangle.coordinates = position
        // set the position as one of the positions that the algorithm has tried to place the triangle at
        // this helps by preventing the algorithm from trying to place the same triangle at the same position
        // and entering an infinite loop. this was the problem for me for a long time
        // this was one of the main issues of the previous algorithms
        triangle.triedPositions.append(position)
        println(s"placed ${triangle.letters} at $position. Remaining triangles: ${unusedTriangles.length}")


      // this is the main loop of the algorithm
      while (unusedTriangles.nonEmpty && !solutionFound) do
        // get a collection of all the fitting triangles
        val fitting = fittingTriangles(unusedTriangles, position)
        if fitting.nonEmpty then
          val triangle = fitting.head
          // if the position that the algorithm has been tried with that triangle
          // then backtrack
          if triangle.triedPositions.contains(position) then
            backtrack(position)
            triangle.triedPositions.clear()
          else
            place(triangle)
          // if there are no more free positions then the solution has been found
          if places.isEmpty then
            solutionFound = true
          else
            position = places.head
        else
          // if there are no fitting triangles then backtrack
          if solutionSet.nonEmpty then
            backtrack(position)
            position = places.head
          else
            solutionFound = false

      println(s"Found ${solutionSet.size} triangles: ${solutionSet.map(_.letters).mkString(", ")}")
      Puzzle.puzzle_triangles.clear()
      solutionFound


  // i've tried everything
  // i've tried everything
  // i've tried everything

  // i am in an endless loop
  // endless loop of triangles
  // endless loop of triangles
  // endless loop of triangles
  // trying to find a solution
  // i am in an endless loop
  // endless loop of debugging
  // endless loop of debugging
  // help me











    




