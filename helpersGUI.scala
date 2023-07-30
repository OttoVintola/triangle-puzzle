package trianglepuzzle

import javafx.scene.image.{Image, ImageView}
import javafx.scene.paint.{Color, Paint}
import o1.grid.GridPos
import scalafx.scene.Group
import scalafx.scene.shape.Polygon
import scalafx.scene.text.{Font, Text}
import trianglepuzzle.Puzzle.{board, current_triangle, last_rotation, numberOfRotations, puzzle_triangles}

import java.io.FileInputStream

object helpersGUI:
  def evenRotations: Group =
    if board.checkRotation(numberOfRotations) then
      Puzzle.last_rotation = true
      show(true)
    else
      Puzzle.last_rotation = false
      show(false)

  // Shows the triangle at the current triangle position
  // If there are no more triangles to show, a red triangle is shown
  def show(rotation: Boolean): Group =
    if puzzle_triangles.nonEmpty then
      place_triangle(GridPos(0, 0), Color.WHITE, Color.BLACK, rotation, current_triangle.letters)
    else
      place_triangle(GridPos(0, 0), Color.DARKRED, Color.DARKRED, rotation, current_triangle.letters)

  // Creates the hexagon grid by fetching a picture and placing it on the scene
  def createHexagon(): ImageView =
     val picture = Image(FileInputStream("hexagon.png"))
     val imageView = ImageView(picture)
     imageView.setX(100)
     imageView.setY(100)
     imageView.setFitWidth(600)
     imageView.setFitHeight(600)
     imageView

  // Message builder (used for file saved, save loaded messages, cheating message and puzzle solved message)
  def showMessage(text: String, where: (Int, Int), size: Double): Text =
    val message = new Text(text)
    message.font = Font("Verdana", size)
    message.layoutX = where._1
    message.layoutY = where._2
    message


  def place_triangle(spot: GridPos, text_color: Paint, triangle_color: Paint, rotate: Boolean, letters: String): Group =
    val polygon = new Polygon
    polygon.points.addAll(0.0, 0.0, 60.0, 120.0, 120.0, 0.0)
    polygon.translateX = Coordinates.toPixel(spot)._1
    polygon.translateY = Coordinates.toPixel(spot)._2
    polygon.rotate = if (rotate) 180.0 else Coordinates.rotation(spot)
    polygon.setFill(triangle_color)

    // helper method to retrieve the coordinates of the triangle based on the index (number of vertices)
    def getTrianglePoints(index: Int): (Double, Double) =
      index match
        case 0 => (polygon.points(0), polygon.points(1))
        case 1 => (polygon.points(2), polygon.points(3))
        case 2 => (polygon.points(4), polygon.points(5))


    // Method to add the letters for example "AbD" to the triangle
    def addText(letter: Char, letterIndex: Int, where: (Int, Int)): Text =
      val text = new Text(letter.toString)
      text.setFill(text_color)
      text.font = Font("Verdana", 20)
      text.translateX = getTrianglePoints(letterIndex)._1 + Coordinates.toPixel(spot)._1 + where._1
      text.translateY = getTrianglePoints(letterIndex)._2 + Coordinates.toPixel(spot)._2 + where._2
      text

    // Checks whether the triangle is rotated or not
    // and based on that decides the correct coordinates for the letters
    val tp_y1 = if (Coordinates.rotation(spot) == 0 && !rotate) 20 else 110
    val tp_x2 = if (Coordinates.rotation(spot) == 0 && !rotate) -20 else 10
    val tp_y2 = if (Coordinates.rotation(spot) == 0 && !rotate) -60 else -50
    val tp_x3 = if (Coordinates.rotation(spot) == 0 && !rotate) -50 else -80
    val tp_y3 = if (Coordinates.rotation(spot) == 0 && !rotate) 60 else 70


    // Adds the 3 letters by calling the methods above
    val text1 = addText(letters(0), 0, (55, tp_y1))
    val text2 = addText(letters(1), 1, (tp_x2, tp_y2))
    val text3 = addText(letters(2), 2, (tp_x3, tp_y3))

    Group(polygon, text1, text2, text3)
  end place_triangle

  // Method to remove the triangle from the board
  // and replace it with an empty triangle
  def remove_triangle(spot: GridPos) =
    val rotation_as_boolean = Coordinates.rotation(spot) == 0
    val empty_triangle = realTriangle(rotation_as_boolean, "fff")
    board.update(spot, empty_triangle)
    val polygon = new Polygon
    polygon.points.addAll(0.0, 0.0, 60.0, 120.0, 120.0, 0.0)
    polygon.translateX = Coordinates.toPixel(spot)._1
    polygon.translateY = Coordinates.toPixel(spot)._2
    polygon.rotate = Coordinates.rotation(spot)
    polygon.setFill(Color.WHITE)
    polygon


