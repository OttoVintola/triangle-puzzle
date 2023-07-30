package trianglepuzzle

import scalafx.scene.shape.Polygon
import o1.grid.GridPos
import scala.collection.mutable.Buffer


class realTriangle(var our_rotation: Boolean, var letters: String):
    var isPlaced: Boolean = false
    var coordinates: GridPos = GridPos(99, 99)

    var triedPositions = Buffer[GridPos]()
