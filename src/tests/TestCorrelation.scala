package tests

import org.scalatest.FunSuite
import recommendations.Recommendations

class TestCorrelation extends FunSuite {

  class TwoDoubles(val a: Double, val b: Double) {

  }

  val EPSILON: Double = 0.01

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Correlation") {

    val list1: List[Int] = List(1,2,3,4,5)
//    val prop1: Int => Double = Int => Double
//
//    Recommendations.correlation(list1, )
//    equalDoubles()

  }

}
