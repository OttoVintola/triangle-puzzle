package trianglepuzzle

import o1.grid.{CompassDir, Grid, GridPos}
import o1.gui.Pic.polygon
import o1.gui.Pos

import scala.collection.mutable.Buffer

import scalafx.application.JFXApp3
import scalafx.Includes.*
import scalafx.scene.shape.Polygon
import scalafx.scene.text.*
import scalafx.scene.Group
import scalafx.scene.layout.Pane
import scalafx.scene.Scene
import javafx.scene.image.*
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent, ScrollEvent}


import java.io.FileInputStream
import javafx.scene.paint.{Color, Paint}
import o1.util.DoNothing


import scalafx.animation.PauseTransition
import scalafx.event.ActionEvent
import javafx.util.Duration


object Puzzle extends JFXApp3:

  // Initialize board
  val board: hexBoard = hexBoard(7, 4)
  board.createTriangles()

  // All of the possible triangles that can be placed
  var puzzle_triangles: Buffer[realTriangle] = board.game_triangles

  // Tracks the state of the puzzle
  var puzzle_solved: Boolean = false

  // Sets the current triangle that is showed
  var current_triangle: realTriangle = puzzle_triangles.head

  // Keeps track of the number of rotations
  var numberOfRotations: Int = 0
  var last_rotation: Boolean = false

  // These variables are for the JSON loading
  // Coordinates for the placed triangles are in loaded_coordinates
  // While the triangles themselves are in toBeLoaded
  var loaded_coordinates: Buffer[GridPos] = Buffer[GridPos]()
  var toBeLoaded: Buffer[realTriangle] = Buffer[realTriangle]()

  // The user can only use the cheat once
  private var cheatUsed = false

  // Checks whether the current triangle needs to be rotated or not
  // Places a triangle based on that on to the game


  // Checks if the place is the reserved empty space outside the visible grid
  private def isVoid(place: GridPos): Boolean = place.x == 6 && place.y == 3


  def start(): Unit =

    // Initial stage settings
    stage = new JFXApp3.PrimaryStage:
      title = "Triangle Puzzle"
      width = 1400
      height = 800

    val root = Pane() // Simple pane component
    root.style = "-fx-background-color: white;"
    // The stack variable represents a Pane() object
    // that holds the stack with the current triangles
    val stack = Pane()
    root.children.add(stack)
    val scene = Scene(parent = root)
    // Scene acts as a container for the scene graph
    stage.scene = scene // Assigning the new scene as the current scene for the stage


    val initialNumber   = helpersGUI.showMessage(s"${puzzle_triangles.size} Triangles left", (780, 250), 24.0)
    val puzzleSolved    = helpersGUI.showMessage("Puzzle Solved! Congratulations", (700, 600), 24.0)
    val cheatingMessage = helpersGUI.showMessage("You used the cheat card", (700, 550), 24.0)

    // Show controls
    val controls = helpersGUI.showMessage(
      "Click to place a triangle \n" +
      "Scroll to remove a triangle \n" +
      "Up/Down to rotate the triangle\n" +
      "Left/Right to shift triangles \n" +
      "'C' to solve the puzzle \n" +
      "'S' to save the current state \n" +
      "'L' to load a saved state \n\n" +
      "Have fun and enjoy! \n", (1100, 70), 16.0)
    root.children += controls

    // Method to set and update the correct number of triangles left in the stack
    def updateNumber(): Unit =
       val numberOfTriangles = helpersGUI.showMessage(s"${puzzle_triangles.size} Triangles left", (780, 250), 24.0)
       root.children.filter(_.getLayoutX == 780).foreach(x => root.children.remove(x))
       root.children += numberOfTriangles


    // Method to remove all triangles from the board
    def clearBoard(): Unit =
            val coordinates = board.triangles.map(_.coordinates)
            coordinates.foreach(* => root.children += helpersGUI.remove_triangle(*))

    // Places a collection of triangles in the given collection coordinates
    // It is used when loading a saved file from JSON and also to use the cheating algorithm
    // Since effectively the same functionality is required for both of the tasks
    // I decided to make it a helper method
    def massPlace(triangles: Buffer[realTriangle], coordinates: Buffer[GridPos]): Unit =
            for i <- coordinates.indices do
              board.update(coordinates(i), triangles(i))
              root.children += helpersGUI.place_triangle(coordinates(i), Color.WHITE, Color.BLACK, false, triangles(i).letters)
              triangles(i).coordinates = coordinates(i)
              triangles(i).isPlaced = true
              board.triangles += triangles(i)


    def placementLogic(place: GridPos): Unit =
        // If the coordinate of the mouse is on the grid
        // And the spot on the board is free
        if !isVoid(place) && board.isFree(place) then
          root.children += helpersGUI.place_triangle(place, Color.WHITE, Color.BLACK, false, current_triangle.letters)
          // set the coordinates of the triangle being placed
          board.moveToBoard(place)
        if puzzle_triangles.nonEmpty then
          board.updateCurrent()
        stack.children.clear()
        stack.children += helpersGUI.evenRotations
        // Check if the puzzle has been solved
        board.checkStatus()

    def removalLogic(place: GridPos): Unit =
       // checks if the mouse action was made on the grid or not
       if !isVoid(place) then
         val triangle_removed = board.elementAt(place)
         // checks if the location where the scroll happened is occupied by a placed triangle
         if !board.isFree(place) then
           root.children += helpersGUI.remove_triangle(place)
           board.moveFromBoard(triangle_removed)
         board.updateCurrent()
         stack.children.clear()
         stack.children += helpersGUI.evenRotations
         board.checkStatus()


    // (Partial) method to set the triangle
    // in the location given by the cursor at the time of the click
    scene.onMouseClicked = (event: MouseEvent) => {
      val place: GridPos = Coordinates.toGridPos(event.sceneX, event.sceneY)
      placementLogic(place)
      updateNumber()
      if puzzle_solved then puzzleSolved.visible = true else puzzleSolved.visible = false
      cheatingMessage.visible = false
    }

    // (Partial) method to remove the triangle in the given spot
    // When mouse wheel is engaged
    scene.onScrollStarted = (event: ScrollEvent) => {
      val place = Coordinates.toGridPos(event.sceneX, event.sceneY)
      removalLogic(place)
      // Updates triangles left number
      updateNumber()
      if puzzle_triangles.nonEmpty then puzzleSolved.visible = false
      cheatingMessage.visible = false
    }


    // Here we have the controls for the game. More on those in the README
    var fileMessage = Text("")
    scene.onKeyPressed = (event: KeyEvent) => {

        event.code match
          case KeyCode.Right =>
            // There needs to be at least two triangles in the stack left
            // for it to make any sense to view the next one (and not get an IndexOutOfBounds exception lol)
            if puzzle_triangles.nonEmpty then
              stack.children.clear()
              board.shiftRight()
              stack.children += helpersGUI.evenRotations
          case KeyCode.Left =>
            if puzzle_triangles.nonEmpty then
              stack.children.clear()
              board.shiftLeft()
              stack.children += helpersGUI.evenRotations
          case KeyCode.Up =>
            if puzzle_triangles.nonEmpty then
              stack.children.clear()
              val Unrotated = helpersGUI.show(false)
              stack.children += Unrotated
              // updates the number of rotations
              if last_rotation then
                numberOfRotations+=1
              // here last_rotation here is updated to mean that Up corresponds to rotation being false
              last_rotation = false
          case KeyCode.Down =>
            if puzzle_triangles.nonEmpty then
              stack.children.clear()
              val rotated = helpersGUI.show(true)
              stack.children += rotated
              if !last_rotation then
                numberOfRotations+=1
              last_rotation = true
          case KeyCode.S =>
            // remove the cheating message if it is visible
            root.children -= cheatingMessage
            cheatingMessage.visible = false
            // Load the triangles into buffer toBePlaced and the leftovers to puzzle_triangles
            JsonHandler.saveTrianglesToJsonFile()
            // Set the previous message visibility to false so that two different messages do not overlap
            fileMessage.visible = false
            fileMessage = helpersGUI.showMessage("File Saved!", (1100, 370), 24.0)
            fileMessage.visible = true
            root.children += fileMessage
            // Handles the text.txt disappearing after 10 seconds
            val pause = new PauseTransition(Duration.seconds(10))
            pause.setOnFinished((_: ActionEvent) => fileMessage.visible = false) // this warning will not go away
            pause.play()
          case KeyCode.L =>
            root.children -= cheatingMessage
            cheatingMessage.visible = false
            // First remove all the triangles from the board
            clearBoard()
            // Set the previous message visibility to false so that two different messages do not overlap
            fileMessage.visible = false
            // Load the triangles and show success message or could not load message
            val triangles = JsonHandler.loadTriangles()
            triangles match
              case result: String => fileMessage = helpersGUI.showMessage(result, (1100, 370), 24.0)
              case _ => fileMessage = helpersGUI.showMessage("Save Loaded!", (1100, 370), 24.0)
            fileMessage.visible = true
            root.children += fileMessage
            // Handles the text.txt disappearing after 10 seconds
            val pause = new PauseTransition(Duration.seconds(10))
            pause.setOnFinished((_: ActionEvent) => fileMessage.visible = false) // this warning will not go away
            pause.play()
            // Set all of the triangles on the board
            massPlace(toBeLoaded, loaded_coordinates)
            // Update the current triangle
            if puzzle_triangles.nonEmpty then
              board.updateCurrent()
            stack.children.clear()
            stack.children += helpersGUI.evenRotations
            // Updates the number of triangles left
            updateNumber()
            // Checks if the puzzle has been solved and based on that sets the visibility of the message
            board.checkStatus()
            if puzzle_solved then puzzleSolved.visible = true else puzzleSolved.visible = false
          case KeyCode.C =>
            // in case there is already a message on the screen, clear it
            root.children -= cheatingMessage
            if !puzzle_solved && !cheatUsed then
               if board.triangles.forall(triangle => Puzzle.board.matches(triangle.coordinates)) then
                   Cheat.solve(puzzle_triangles, Cheat.freePositions.toBuffer)
               else
                  clearBoard()
                  for triangle <- board.triangles do
                      puzzle_triangles += triangle
                  board.triangles.clear()
                  Cheat.solve(puzzle_triangles, Cheat.freePositions.toBuffer)
               // put the board triangles in the puzzle triangles so that the algorithm can place them all
               // Actual call for the algorithm to solve the puzzle
               // GUI workings of the program to show the solution
               massPlace(Cheat.solutionSet, Cheat.freePositions.toBuffer)
               board.triangles.clear()
               board.triangles ++= Cheat.solutionSet
               Cheat.solutionSet.clear()
               // Updates current number of triangles
               updateNumber()
               // Checks if the puzzle has been solved and based on that sets the visibility of the message
               board.checkStatus()
               if puzzle_solved then puzzleSolved.visible = true else puzzleSolved.visible = false
               // Shows the red triangle since there are no more triangles left
               stack.children.clear()
               stack.children += helpersGUI.evenRotations
               root.children += cheatingMessage
               cheatingMessage.visible = true
               cheatUsed = true
          case _ => DoNothing
    }

    // Sets the initial triangle
    stack.children += helpersGUI.evenRotations

    // Puzzle solved text.txt
    root.children += puzzleSolved
    puzzleSolved.visible = false

    // Sets initial number of triangles
    root.children += initialNumber

    // Creates the grid
    root.children += helpersGUI.createHexagon()



