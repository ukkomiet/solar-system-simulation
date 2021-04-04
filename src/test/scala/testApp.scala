

import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.shape._
import scalafx.scene.effect._
import scalafx.scene.paint._
import scalafx.event._


object testApp extends JFXApp {

  stage = new JFXApp.PrimaryStage {
    title = "testApp"
    scene = new Scene(1700,900) {
      fill = Color.rgb(140,140,140)
      val circle = new Circle()
      circle.centerX = 850
      circle.centerY = 450
      circle.radius = 30
      circle.fill = Color.rgb(240,240,240)
      content = List(circle)
    }
  }
}


