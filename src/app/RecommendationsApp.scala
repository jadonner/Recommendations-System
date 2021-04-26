package app

import recommendations.Recommendations.topKCorrelations

import scala.io.Source

object RecommendationsApp {

  val numberOfRecommendations: Int = 6

  def getStat(item: String): Customer => Double = {
    c: Customer => c.purchases(item)
  }

  def recommendations(filename: String, viewingItem: String): List[String] = {
    val lines = Source.fromFile(filename).getLines()
    val header = lines.next()
    val itemNames = header.split(",")
    val customers: List[Customer] = (for (line <- lines) yield {
      new Customer({
        val values = line.split(",")
        (itemNames zip values.map(_.toInt)).toMap
      })
    }).toList

    topKCorrelations(customers, (itemNames zip itemNames.map(getStat)).toMap, viewingItem, numberOfRecommendations)


  }

}
