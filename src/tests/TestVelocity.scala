package tests

import maze.{GridLocation, PhysicsVector}
import org.scalatest.FunSuite
import solver.PathFinding.getVelocity

class TestVelocity extends FunSuite {

  val EPSILON: Double = 0.01

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  def equalsVectors(v1: PhysicsVector, v2: PhysicsVector): Boolean = {
    equalDoubles(v1.x, v2.x) && equalDoubles(v1.y, v2.y) && equalDoubles(v1.z, v2.z)
  }


  test("test pathToDirection 1") {

    // Testing Opportunity 1: Path to Velocity
    // Sample Test Case 1
    // A path of (1,2) -> (2,2) -> (3,2)
    // A location of (1.5, 2.5), which is the center of the first tile in the path
    val result1: PhysicsVector = new PhysicsVector(5, 0, 0)
    val path1: List[GridLocation] = List(
      new GridLocation(1, 2),
      new GridLocation(2, 2),
      new GridLocation(3, 2)
    )

    val velocity1: PhysicsVector = getVelocity(path1, new PhysicsVector(1.5, 2.5))
    assert(equalsVectors(velocity1, result1))

    // Sample Test Case 2
    // A path of (11,12) -> (10,12) -> (9,12) -> (9,13) -> (9,14) -> (10,14)
    // A location of (9.55, 14.1)
    val result2: PhysicsVector = new PhysicsVector(4.61, 1.94, 0)
    val path2: List[GridLocation] = List(
      new GridLocation(11, 12),
      new GridLocation(10, 12),
      new GridLocation(9, 12),
      new GridLocation(9, 13),
      new GridLocation(9, 14),
      new GridLocation(10, 14)
    )

    val velocity2: PhysicsVector = getVelocity(path2, new PhysicsVector(9.55, 14.1))
    assert(equalsVectors(velocity2, result2))

    // Sample Test Case 3
    // A path of (11,12) -> (10,12) -> (9,12) -> (9,13) -> (9,14) -> (10,14)
    // A location of (10.48, 14.49)
    val result3: PhysicsVector = new PhysicsVector(0, 0, 0)
    val path3: List[GridLocation] = List(
      new GridLocation(11, 12),
      new GridLocation(10, 12),
      new GridLocation(9, 12),
      new GridLocation(9, 13),
      new GridLocation(9, 14),
      new GridLocation(10, 14)
    )

    val velocity3: PhysicsVector = getVelocity(path3, new PhysicsVector(10.48, 14.49))
    assert(equalsVectors(velocity3, result3))

    // Sample Test Case 4
    // A path of (11,12) -> (10,12) -> (9,12) -> (9,13) -> (9,14) -> (10,14)
    // A location of (10.9, 14.52)
    val result4: PhysicsVector = new PhysicsVector(-5, 0, 0)
    val path4: List[GridLocation] = List(
      new GridLocation(11, 12),
      new GridLocation(10, 12),
      new GridLocation(9, 12),
      new GridLocation(9, 13),
      new GridLocation(9, 14),
      new GridLocation(10, 14)
    )

    val velocity4: PhysicsVector = getVelocity(path4, new PhysicsVector(10.9, 14.52))
    assert(equalsVectors(velocity4, result4))

  }


}
