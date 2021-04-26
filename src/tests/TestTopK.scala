package tests

import org.scalatest.FunSuite
import recommendations.Recommendations

class TestTopK extends FunSuite {

  val EPSILON: Double = 0.01

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Top K") {

//    Recommendations.topKCorrelations()

  }

}
