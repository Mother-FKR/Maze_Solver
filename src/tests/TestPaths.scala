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
    val result1: List[GridLocation] = List(
      new GridLocation(3,12),
      new GridLocation(3,11),
      new GridLocation(3,10),
      new GridLocation(3,9),
      new GridLocation(3,8)
    )

    val path1 = findPath(new GridLocation(3, 12), new GridLocation(3, 8), TestingMaps.apply(0).tiles)

    for (i <- result1.indices) {
      assert(path1(i).x == result1(i).x && path1(i).y == result1(i).y)
    }

    // Sample Test Case 2
    // Start: (5,5)
    // End: (8,8)
    // The optimal path is (5,5) -> (5,6) -> (5,7) -> (5,8) -> (6,8) -> (7,8) -> (8,8)
    val result2: List[GridLocation] = List(
      new GridLocation(5, 5),
      new GridLocation(5, 6),
      new GridLocation(5, 7),
      new GridLocation(5, 8),
      new GridLocation(6, 8),
      new GridLocation(7, 8),
      new GridLocation(8, 8)
    )

    val path2 = findPath(new GridLocation(5, 5), new GridLocation(8, 8), TestingMaps.apply(0).tiles)

    for (i <- result2.indices) {
      assert(path2(i).x == result2(i).x && path2(i).y == result2(i).y)
    }

    // Sample Test Case 3
    // Start: (5,5)
    // End: (5,8)
    // The optimal path is (5,5) -> (5,6) -> (5,7) -> (5,8)
    val result3: List[GridLocation] = List(
      new GridLocation(5, 5),
      new GridLocation(5, 6),
      new GridLocation(5, 7),
      new GridLocation(5, 8)
    )

    val path3 = findPath(new GridLocation(5, 5), new GridLocation(5, 8), TestingMaps.apply(0).tiles)

    for (i <- result3.indices) {
      assert(path3(i).x == result3(i).x && path3(i).y == result3(i).y)
    }

    // Testing Opportunity 3: Valid Paths
    // Sample Test Case 1
    // Use TestingMap level 1 (This map contains impassable tiles)
    // Start: (15, 10)
    // End: (15, 4)
    val result4: List[GridLocation] = List(
      new GridLocation(15, 10),
      new GridLocation(15, 11),
      new GridLocation(15, 12),
      new GridLocation(15, 13),
      new GridLocation(14, 13),
      new GridLocation(13, 13),
      new GridLocation(12, 13),
      new GridLocation(11, 13),
      new GridLocation(10, 13),
      new GridLocation(9, 13),
      new GridLocation(8, 13),
      new GridLocation(8, 12),
      new GridLocation(8, 11),
      new GridLocation(8, 10),
      new GridLocation(8, 9),
      new GridLocation(8, 8),
      new GridLocation(8, 7),
      new GridLocation(8, 6),
      new GridLocation(8, 5),
      new GridLocation(9, 5),
      new GridLocation(9, 4),
      new GridLocation(10, 4),
      new GridLocation(11, 4),
      new GridLocation(12, 4),
      new GridLocation(13, 4),
      new GridLocation(14, 4),
      new GridLocation(15, 4),
    )

    val path4 = findPath(new GridLocation(15, 10), new GridLocation(15, 4), TestingMaps.apply(1).tiles)
    for (i <- result4.indices) {
      assert(path4(i).x == result4(i).x && path4(i).y == result4(i).y)
    }

  }




}

