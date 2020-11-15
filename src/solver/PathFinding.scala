package solver

import maze.{GridLocation, MapTile, PhysicsVector}

import scala.collection.mutable
import scala.collection.mutable.Stack


object PathFinding {

  def findPath(start: GridLocation, end: GridLocation, map: List[List[MapTile]]): List[GridLocation] = {

    val pathInGridLocation: List[GridLocation] = List()
    val filedMap: List[List[Int]] = mapFilter(map, "ground")
    val adjacencyList: Map[List[Int], List[List[Int]]] = getAdjacencyList(filedMap)
    val distance: Map[List[Int], Int] = getDistance(start, adjacencyList)
    println(distance)
    val path: List[List[Int]] = getPath(List(end.x, end.y), adjacencyList, distance)

    for (tile <- path) {
      pathInGridLocation :+ List(tile.head, tile(1))
    }

    pathInGridLocation
  }

  def getVelocity(path: List[GridLocation], currentLocation: PhysicsVector): PhysicsVector = {
    null
  }


  // Helper Method :)

  /***
   *
   * @param map the input, which is the map we need to work on.
   * @return return a Map: [Node's position -> It's neighbors]
   */
  def getAdjacencyList(map: List[List[Int]]): Map[List[Int], List[List[Int]]] = {

    var adjacencyList: Map[List[Int], List[List[Int]]] = Map()

    for (tile <- map) {

      val x: Int = tile.head // Get tile's X coordinate
      val y: Int = tile(1) // Get tile's Y coordinate
      var neighbors: List[List[Int]] = List()

      // Up
      if (map.contains(List(x + 1, y))) {
        neighbors = neighbors :+ List(x + 1, y)
      }

      // Down
      if (map.contains(List(x - 1, y))) {
        neighbors = neighbors :+ List(x - 1, y)
      }

      // Right
      if (map.contains(List(x, y + 1))) {
        neighbors = neighbors :+ List(x, y + 1)
      }

      // Left
      if (map.contains(List(x, y - 1))) {
        neighbors = neighbors :+ List(x, y - 1)
      }

      // Return
      adjacencyList += tile -> neighbors
    }
    adjacencyList
  }

  /***
   *
   * @param map the origin map with wall & ground
   * @param filterObject the object you want to filter
   * @return return the map with only ground
   */
  def mapFilter(map: List[List[MapTile]], filterObject: String): List[List[Int]] = {

    val maxX: Int = map.length
    val maxY: Int = map.head.length
    var result: List[List[Int]] = List()

    for (x <- 0 until maxX) {
      for (y <- 0 until maxY) {
        if (map.apply(x).apply(y).tileType == filterObject) {
          result = result :+ List(x, y)
        }
      }
    }
    result
  }

  /***
   *
   * @param map the map we need to work on
   * @param start the start tile
   * @param adjacencyList all neighbors
   * @return all tile's distance from start tile
   */
  def getDistance(start: GridLocation, adjacencyList: Map[List[Int], List[List[Int]]]): Map[List[Int], Int] = {

    val startPoint: List[Int] = List(start.x, start.y)

    var explored: Set[List[Int]] = Set(startPoint)

    var distances: Int = 1

    var distance: Map[List[Int], Int] = Map()
    distance += startPoint -> 0

    val toExplore: Stack[List[Int]] = new Stack[List[Int]]()
    toExplore.push(startPoint)

    while (toExplore.nonEmpty) {
      val tileToExplore = toExplore.pop()
      for (neighbor <- adjacencyList(tileToExplore)) {
        if (!explored.contains(neighbor)) {
          distance += neighbor -> distances
          toExplore.push(neighbor)
          explored = explored + neighbor
        }
      }
      distances += 1
    }

    distance

  }

  def getPath(end: List[Int], adjacencyList: Map[List[Int], List[List[Int]]], distance: Map[List[Int], Int]): List[List[Int]] = {

    var pathDistance: Int = distance(end) - 1

    var path = List(end)

    while (pathDistance > 0) {
      for (tile <- distance.keys) {
        if (adjacencyList(path.head).contains(tile)) {
          path = tile +: path
          pathDistance -= 1
        } else {
          return path
        }
      }
    }
    path
  }

}
