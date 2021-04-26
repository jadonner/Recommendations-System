package recommendations

import scalafx.scene.control.Label

object Recommendations {

  def standardDeviation[T](elements: List[T], property: T => Double): Double = {
    val listDoubles: List[Double] = for (items <- elements) yield {
      property(items)
    }
    val listAvg: Double = listDoubles.sum / listDoubles.length
    val averageNumbersSqrd: List[Double] = for (num <- listDoubles) yield {
      Math.pow(num - listAvg, 2)
    }
    Math.sqrt(averageNumbersSqrd.sum / (listDoubles.length - 1))
  }


  // population or sample? sample coded here
  // higher correlation means items are frequently bought together
  def correlation[T](elements: List[T], property1: T => Double, property2: T => Double): Double = {
    val listProperty1: List[Double] = for (items <- elements) yield {
      property1(items)
    }
    val listProperty2: List[Double] = for (items <- elements) yield {
      property2(items)
    }
    val list1avg: Double = listProperty1.sum / listProperty1.length
    val list2avg: Double = listProperty2.sum / listProperty2.length
//    val total1: List[Double] = for (data1 <- listProperty1) yield {
//      data1 - list1avg
//    }
//    val total2: List[Double] = for (data2 <- listProperty2) yield {
//      data2 - list2avg
//    }
//      (total1 * total2.sum) / elements.length
    val covariance: Double = (for (num <- listProperty1.indices) yield (listProperty1(num) - list1avg) * (listProperty2(num) - list2avg)).sum / (listProperty2.length - 1)

    //covariance / sd1 * sd2
    covariance / (standardDeviation(elements, property1) * standardDeviation(elements, property2))
  }


  def topKCorrelations[T](elements: List[T], allProperties: Map[String, T => Double], testPropertyName: String, k: Int): List[String] = {
    //this should be all of the product names mapped to its correlation to the test product
    val correlationMap: Map[String, Double] = (for (productNames <- allProperties.keys) yield{
      productNames -> correlation(elements, allProperties(productNames), allProperties(testPropertyName))
    }).toMap
    val mapList: List[(String, Double)] = correlationMap.toList.sortWith(_._2 > _._2)
    //mapList.foreach(println)
    val returnList: List[String] = (for (numberOfReccommendations <- 1 to k) yield {
      mapList(numberOfReccommendations)._1
    }).toList
    returnList
  }

}
