package tests

import org.scalatest.FunSuite
import recommendations.Recommendations

class TestStdDev extends FunSuite {

  val EPSILON: Double = 0.01

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Standard Deviation") {

    val data: List[Double] = List(7.63, 3.87, 1.59, 8.26, 5.11, 0.65, 7.88)

    val stdDev: Double = Recommendations.standardDeviation(data, (d: Double) => d)
    val expected: Double = 3.100496888

    assert(equalDoubles(stdDev, expected), "\nexpected: " + expected + "\ncomputed: " + stdDev)
  }


}

