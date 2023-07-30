package trianglepuzzle

import o1.grid.CompassDir.{East, North, South, West}
import o1.grid.{CompassDir, Grid, GridPos}
import trianglepuzzle.Puzzle.{board, current_triangle, numberOfRotations, puzzle_solved, puzzle_triangles}

import scala.collection.mutable.Buffer
import scala.util.Random



class hexBoard(val b_width: Int, val b_height: Int) extends Grid[realTriangle](b_width, b_height):
  // The triangle texts in an array grouped
  // Example Array[String]("AAA", "abd".....)
  private val triangle_texts: Vector[String] = solutionReader.readFile.filterNot(_.isWhitespace).grouped(3).toVector

  // All triangles that have been placed on the board
  var triangles: Buffer[realTriangle] = Buffer()

  // All the possible triangles that are in the game
  // Mainly for unit testing
  val game_triangles: Buffer[realTriangle] = Buffer()

  override def initialElements: Seq[realTriangle] =
    val empty_triangle = realTriangle(false, "fff")
    val empty_triangles: Seq[realTriangle] = Seq.fill(28)(empty_triangle)
    empty_triangles

  // Returns a boolean based on how many rotations have been made
  def checkRotation(numberOfRotations: Int): Boolean = numberOfRotations%2 == 0

  // Creates all the triangles for the game
  def createTriangles(): Unit =
    val triangle_text: Vector[String] = triangle_texts

    def addTriangle(rotation: Boolean, index: Int) =
      game_triangles.append(realTriangle(rotation, triangle_text(index)))

    def create_first(): Unit =
       for i <- 0 to 4 do
         if i%2 == 0 then
           addTriangle(true, i)
         else
           addTriangle(false, i)
    def create_middle(): Unit =
      for i <- 5 to 19 do
        if i%2 == 0 then
            addTriangle(false, i)
        else
            addTriangle(true, i)
    def create_last(): Unit =
       for i <- 20 to 23 do
         if i%2 == 0 then
           addTriangle(false, i)
         else
           addTriangle(true, i)
    create_first()
    create_middle()
    create_last()
  end createTriangles

  def isFree(spot: GridPos): Boolean = !this.elementAt(spot).isPlaced

  // moveToBoard and moveFromBoard are used to move the triangle from the puzzle to the board
  def moveToBoard(place: GridPos): Unit =
      Puzzle.board.update(place, Puzzle.current_triangle)
      Puzzle.current_triangle.coordinates = place
      current_triangle.isPlaced = true
      puzzle_triangles -= Puzzle.current_triangle
      Puzzle.board.triangles += Puzzle.current_triangle

  def moveFromBoard(triangle: realTriangle): Buffer[realTriangle] =
      triangle.isPlaced = false
      Puzzle.puzzle_triangles += triangle
      Puzzle.board.triangles -= triangle

  def updateCurrent(): Unit = current_triangle = puzzle_triangles.head

  // Method to shift the triangle stack to the left
  def shiftLeft(): Unit =
    val position = puzzle_triangles.indexOf(current_triangle)
    if position != 0 then
      current_triangle = puzzle_triangles((position - 1)%24)
    else
      current_triangle = puzzle_triangles.last

  // Method to shift the triangle stack to the right
  def shiftRight(): Unit =
    val position = puzzle_triangles.indexOf(current_triangle)
    if position != puzzle_triangles.size-1 then
      current_triangle = puzzle_triangles((position + 1)%24)
    else
      current_triangle = puzzle_triangles.head


  // Checks if the puzzle is solved or not
  // this was in the Puzzle object
  // but since it does not have anything to do with the GUI
  // I moved it here
  // Check if puzzle_triangles is empty first
  // So, that the program does not waste a lot of computations checking if
  // Every triangle matches only to find out that the board is not completely filled up
  // This is made possible with the Boolean logic operator evaluating order
  // If puzzle_triangles.isEmpty returns a false value then the expression is automatically false, since 0 && (1 or 0) is false
  def checkStatus(): Unit = Puzzle.puzzle_solved = Puzzle.puzzle_triangles.isEmpty && board.triangles.forall(triangle => board.matches(triangle.coordinates))

  // This method broke a few days ago
  // So, this has been completely rewritten
  // It got a little bloated and messy, but it works
  def matches(place: GridPos): Boolean =
    val this_letters = this.elementAt(place).letters
    val triangles_around = this.neighbors(place, false)
    val noRotation: Boolean = Coordinates.rotation(place) == 0
    def checkLetters(a: Char, b: Char): Boolean = (a.toLower == b.toLower && a != b) || a == 'f'

    // Helpers to check each direction individually
    def checkNorth(spot: GridPos) =
      val northLetters = board.elementAt(spot.neighbor(North)).letters
      checkLetters(northLetters(0), this_letters(0))
    def checkEast(spot: GridPos) =
      val eastLetters = board.elementAt(spot.neighbor(East)).letters
      val notRotated = Coordinates.rotation(spot) == 0
      var check = false
      if notRotated then
        check = checkLetters(eastLetters(2), this_letters(2)) //(eastLetters(2) == this_letters(2) || eastLetters(2) ==  'f')
      else
        check = checkLetters(eastLetters(1), this_letters(1)) //(eastLetters(1) == this_letters(1) || eastLetters(1) == 'f')
      check
    def checkWest(spot: GridPos) =
      val westLetters = board.elementAt(spot.neighbor(West)).letters
      val notRotated = Coordinates.rotation(spot) == 0
      var check = false
      if notRotated then
        check = checkLetters(westLetters(1), this_letters(1)) //westLetters(1) == this_letters(1) || westLetters(2) ==  'f')
      else
        check = checkLetters(westLetters(2), this_letters(2)) //westLetters(2).toLower == this_letters(2).toLower && westLetters(2) != this_letters(2) || westLetters(1) == 'f')
      check
    def checkSouth(spot: GridPos) =
      val southLetters = board.elementAt(spot.neighbor(South)).letters
      checkLetters(southLetters(0), this_letters(0))

    // This is the shortest match case structure I could come up with
    // It checks the position of the triangle and the rotation
    // Then it checks the neighbors in the correct direction
    val check = (place, noRotation) match
      case (GridPos(0, 0), true) => checkWest(place) && checkEast(place)
      case (GridPos(0, 0), false) => checkEast(place) && checkSouth(place) && checkWest(place)
      case (GridPos(0, 1), _) => checkSouth(place) && checkEast(place)
      case (GridPos(6, 1), _) => checkSouth(place) && checkWest(place)
      case (GridPos(_, 1), true) => checkNorth(place) && checkWest(place) && checkEast(place)
      case (GridPos(_, 1), false) => checkSouth(place) && checkWest(place) && checkEast(place)
      case (GridPos(0, 2), _) => checkNorth(place) && checkEast(place)
      case (GridPos(6, 2), _) => checkNorth(place) && checkWest(place)
      case (GridPos(_, 2), true) => checkNorth(place) && checkEast(place) && checkWest(place)
      case (GridPos(_, 2), false) => checkSouth(place) && checkEast(place) && checkWest(place)
      case (GridPos(_, 3), true) => checkNorth(place) && checkEast(place) && checkWest(place)
      case _ => checkEast(place) && checkWest(place)
    check















