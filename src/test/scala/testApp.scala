

import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.shape._
import scalafx.scene.effect._
import scalafx.scene.paint._
import scalafx.event._
import scalafx.animation._
import scalafx.scene.canvas._
import scalafx.scene.layout.BorderPane

object testApp extends JFXApp {


  stage = new JFXApp.PrimaryStage {
    title = "testApp"
    scene = new Scene(1700,900) {
      val canvas = new Canvas(1700,900)
      val gc = canvas.graphicsContext2D
      val border = new BorderPane
      border.center = canvas
      root = border
      fill = Color.rgb(140,140,140)
      val circle = new Circle()
      circle.centerX = 850
      circle.centerY = 450
      circle.radius = 30
      circle.fill = Color.rgb(240,240,240)
      // content = List(circle)
      val space = new Space

      val timer = AnimationTimer { time =>
        gc.fill = Color.White
        gc.fillOval(star.pos.x,star.pos.y,star.radius,star.radius)
        gc.fillOval(planet.pos.x,planet.pos.y,planet.radius,planet.radius)
        space.advanceStepInTime()
      }
      timer.start()
      canvas.requestFocus()

      val star = new AstralBody(space, "Helmeri",10, 20, new Vector3((canvas.width/2).toDouble, (canvas.height/2).toDouble, 0), new Vector3(0,0,0))
      val planet = new AstralBody(space, "Pikku Myy", 10, 15, new Vector3(650,450,0), new Vector3(0,0,0))
      space.addBody(star)
      space.addBody(planet)
      space.setTimeStep(2)

    }
  }
}


