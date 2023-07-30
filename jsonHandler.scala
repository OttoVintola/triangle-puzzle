package trianglepuzzle


import io.circe
import io.circe.*
import io.circe.syntax.*
import io.circe.generic.auto.*


import o1.grid.GridPos

import java.io.*
import scala.collection.mutable.Buffer
import javafx.scene.paint.Color


// Helper to turn a GridPos into a tuple
def gridToInt(spot: GridPos): (Int, Int) = (spot.x, spot.y)

object JsonHandler:
  private case class Triangles(placed: List[String], left: List[String], places: List[(Int, Int)])

  // placed is the triangles that are already placed in the puzzle
  private val placed = Puzzle.board.triangles.map(_.letters)
  // left is just the triangles that are left in the puzzle or in other words; the ones that are not placed
  private val left = Puzzle.puzzle_triangles.map(_.letters)
  // places is the coordinates of the placed triangles
  private val places = Puzzle.board.triangles.map(triangle => gridToInt(triangle.coordinates))

  // Saves the current triangles to a JSON file
  def saveTrianglesToJsonFile(): Unit =
    val wrapper = Triangles(placed.toList, left.toList, places.toList)
    val json = wrapper.asJson
    val writer = new PrintWriter(new File("src/main/scala/trianglepuzzle/solutions.JSON"))
    writer.write(json.spaces2)
    writer.close()

  // Reads the saved triangles from the JSON file
  private def readTrianglesFromJsonFile(): Option[Triangles] =
    val source = scala.io.Source.fromFile("src/main/scala/trianglepuzzle/solutions.JSON")
    val jsonString = source.getLines().mkString
    source.close()
    val json = circe.parser.parse(jsonString).getOrElse(Json.Null)
    json.as[Triangles].toOption

  // Loads the saved triangles from the JSON file
  private val maybeTriangles = readTrianglesFromJsonFile()

  def loadTriangles()  =
    maybeTriangles match
      case None => helpersGUI.showMessage("Error reading JSON data from file.", (1100, 370), 24.0)
      case Some(triangles) =>
        // Access the buffers from the decoded Triangles object
        val placed = Buffer.from(triangles.placed)
        val left = Buffer.from(triangles.left)
        val places = Buffer.from(triangles.places.map(p => GridPos(p._1, p._2)))
        // Updates the collections in Puzzle object
        Puzzle.toBeLoaded = placed.map(text => realTriangle(false, text))
        Puzzle.puzzle_triangles = left.map(text => realTriangle(false, text))
        Puzzle.loaded_coordinates = places










