package app

import javafx.event.ActionEvent
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.VBox

import scala.collection.JavaConverters._
import scala.io.Source

object GUI extends JFXApp {
  val dataFile: String = "data/OnlineRetail.csv"
  val inputLabel: Label = new Label("View Item:")
  val options: ComboBox[String] = new ComboBox(Source.fromFile(dataFile).getLines().next().split(","))
  val wordsLabel: Label = new Label("Recommendations:")

  val outputRecommendations: ListView[String] = new ListView[String]() {
    minHeight = 200
    maxHeight = Double.MaxValue
  }

  val container: VBox = new VBox(inputLabel, options, wordsLabel, outputRecommendations)

  container.children.asScala.foreach(_.setStyle("-fx-font: 24 Arial;"))

  this.stage = new PrimaryStage {
    title = "Recommendations"
    scene = new Scene(container, 600, 400) {
      minWidth = 600
      minHeight = 400
    }
  }

  options.onAction = (_: ActionEvent) => {
    outputRecommendations.getItems.clear()
    outputRecommendations.getItems.addAll(RecommendationsApp.recommendations(dataFile, options.getValue).asJava)
  }
}
