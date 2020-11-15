package tests

import maze.GridLocation
import org.scalatest._
import solver.PathFinding.findPath

class TestPaths extends FunSuite {


  test("test path 1") {
    val level: Int = 0
    TestingMaps(level).tiles

    // Testing Opportunity 2: Connected Paths
    // Sample Test Case 1
    // Start: (3,12)
    // End: (3,8)
    // The optimal path is (3,12) -> (3,11) -> (3,10) -> (3,9) -> (3,8)

    val path1 = findPath(new GridLocation(3, 12), new GridLocation(3, 8), TestingMaps.apply(0).tiles)
    val x = 1
    assert(findPath(new GridLocation(3, 12), new GridLocation(3, 8), TestingMaps.apply(0).tiles).head.x == 3)
  }




}

