package solver

import maze.{GridLocation, MapTile, PhysicsVector}

import scala.collection.mutable


object PathFinding {

  def findPath(start: GridLocation, end: GridLocation, map: List[List[MapTile]]): List[GridLocation] = {

    val pathInGridLocation: List[GridLocation] = List()
    val filedMap: List[List[Int]] = mapFilter(map, "ground")
    val adjacencyList: Map[List[Int], List[List[Int]]] = getAdjacencyList(filedMap)
    val distance: Map[List[Int], Int] = getDistance(start, adjacencyList)
    val path: List[List[Int]] = getPath(List(end.x, end.y), adjacencyList, distance)

    for (tile <- path) {
      pathInGridLocation :+ new GridLocation(tile.head, tile(1))
    }

    pathInGridLocation
  }

  def getVelocity(path: List[GridLocation], currentLocation: PhysicsVector): PhysicsVector = {
    val currentInPath: GridLocation = new GridLocation(currentLocation.x.toInt, currentLocation.y.toInt)
    val nextTileCenter: PhysicsVector = new PhysicsVector(0, 0, 0)
    val vector: PhysicsVector = new PhysicsVector(0, 0, 0)
    var distance2d: Double = 0.0

    if (currentInPath.x == path.last.x && currentInPath.y == path.last.y) {
      nextTileCenter.x = path.last.x + 0.5
      nextTileCenter.y = path.last.y + 0.5

      vector.x = nextTileCenter.x - currentLocation.x
      vector.y = nextTileCenter.y - currentLocation.y

      distance2d = nextTileCenter.distance2d(currentLocation)
      if (distance2d < 0.1) {
        new PhysicsVector(0, 0, 0)
      } else {
        val velocity: PhysicsVector = vector.normal2d()
        velocity.x = velocity.x * 5
        velocity.y = velocity.y * 5
        velocity
      }
    } else {
      for (n <- path.indices) {
        if (path(n).x == currentInPath.x && path(n).y == currentInPath.y) {
          nextTileCenter.x = path(n + 1).x + 0.5 // get X coordinates
          nextTileCenter.y = path(n + 1).y + 0.5 // get Y coordinates
        } else {
          println("Does not Find the Current Location on path!")
          null
        }
      }

      vector.x = nextTileCenter.x - currentLocation.x
      vector.y = nextTileCenter.y - currentLocation.y

      val velocity: PhysicsVector = vector.normal2d()

      velocity.x = velocity.x * 5
      velocity.y = velocity.y * 5

      velocity
    }

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
   * @param start the start tile
   * @param adjacencyList all neighbors
   * @return all tile's distance from start tile
   */
  def getDistance(start: GridLocation, adjacencyList: Map[List[Int], List[List[Int]]]): Map[List[Int], Int] = {

    val startPoint: List[Int] = List(start.x, start.y)
    var explored: Set[List[Int]] = Set(startPoint)
    var distances: Int = 1
    var level: List[List[List[Int]]] = List(List(startPoint))
    var distance: Map[List[Int], Int] = Map()
    distance += startPoint -> 0
    val toExplore: mutable.Queue[List[Int]] = new mutable.Queue()
    toExplore.enqueue(startPoint)

    while (toExplore.nonEmpty) {

      val tileToExplore = toExplore.dequeue()

      //println(tileToExplore + "has neighbor-> " + adjacencyList(tileToExplore))

      for (neighbor <- adjacencyList(tileToExplore)) {
        if (!explored.contains(neighbor)) {

          //println("exploring: " + neighbor + ", distance: " + distances)

          for (n <- level.indices) {
            var levelCollector: List[List[Int]] = List()
            for (levelTile <- level(n)) {
              if (adjacencyList(levelTile).contains(neighbor)) {
                //println("neighbor: " + adjacencyList(levelTile) + ", has " + neighbor)
                distances = n + 1
                levelCollector = levelCollector ::: adjacencyList(levelTile)
              }
            }
            level = level :+ levelCollector
          }

          distance += neighbor -> distances
          toExplore.enqueue(neighbor)
          explored = explored + neighbor
        }
      }
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
