package trianglepuzzle

import o1.grid.GridPos
import scala.math.pow
import scala.math.abs

// This is where all the monkey brain 3 AM code lives in
// Big disappointment that this isnt handled more elegantly
// This would all be avoided if my grid was symmetrical
object Coordinates:

  // Maps Grid coordinates to pixels
  def toPixel(spot: GridPos): (Double, Double) =
    spot match
      case GridPos(1, 0) => (194, 131)
      case GridPos(2, 0) => (265, 125)
      case GridPos(3, 0) => (337, 133)
      case GridPos(4, 0) => (409, 125)
      case GridPos(5, 0) => (480, 133)
      case GridPos(0, 1) => (124, 272)
      case GridPos(1, 1) => (195, 267)
      case GridPos(2, 1) => (266, 272)
      case GridPos(3, 1) => (338, 267)
      case GridPos(4, 1) => (409, 273)
      case GridPos(5, 1) => (480, 268)
      case GridPos(6, 1) => (553, 273)
      case GridPos(0, 2) => (123, 408)
      case GridPos(1, 2) => (195, 413)
      case GridPos(2, 2) => (267, 408)
      case GridPos(3, 2) => (338, 413)
      case GridPos(4, 2) => (410, 409)
      case GridPos(5, 2) => (481, 414)
      case GridPos(6, 2) => (552, 409)
      case GridPos(1, 3) => (195, 548)
      case GridPos(2, 3) => (267, 555)
      case GridPos(3, 3) => (339, 548)
      case GridPos(4, 3) => (411, 555)
      case GridPos(5, 3) => (482, 549)
      case GridPos(0, 0) => (800, 300)
      case GridPos(7, 4) => (800, 300)
      case GridPos(6, 3) => (-200, -200)


  def toGridPos(x: Double, y: Double): GridPos =
    val xs = 25
    val ys = 50
    val gridPosMap = Map(
    (250, 195) -> GridPos(1, 0),
    (320, 195) -> GridPos(2, 0),
    (390, 195) -> GridPos(3, 0),
    (470, 195) -> GridPos(4, 0),
    (540, 195) -> GridPos(5, 0),
    (180, 335) -> GridPos(0, 1),
    (250, 335) -> GridPos(1, 1),
    (325, 335) -> GridPos(2, 1),
    (400, 335) -> GridPos(3, 1),
    (470, 340) -> GridPos(4, 1),
    (550, 340) -> GridPos(5, 1),
    (620, 340) -> GridPos(6, 1),
    (180, 465) -> GridPos(0, 2),
    (250, 465) -> GridPos(1, 2),
    (320, 465) -> GridPos(2, 2),
    (395, 465) -> GridPos(3, 2),
    (465, 465) -> GridPos(4, 2),
    (550, 465) -> GridPos(5, 2),
    (610, 465) -> GridPos(6, 2),
    (250, 600) -> GridPos(1, 3),
    (320, 600) -> GridPos(2, 3),
    (390, 600) -> GridPos(3, 3),
    (465, 600) -> GridPos(4, 3),
    (540, 600) -> GridPos(5, 3)
    )
    gridPosMap.find { case ((gx, gy), _) => abs(x - gx) <= xs && abs(y - gy) <= ys }
      .map(_._2)
      .getOrElse(GridPos(6, 3))


  def rotation(spot: GridPos): Double =
    if ((spot.y == 0 && spot.x % 2 == 0) || (spot.y == 1 && spot.x % 2 != 0) ||
        (spot.y == 2 && spot.x % 2 == 0) || (spot.y == 3 && spot.x % 2 != 0)) 0
    else 180.0




