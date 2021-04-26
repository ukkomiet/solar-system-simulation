

import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.shape._
import scalafx.scene.text._
import scalafx.scene.effect._
import scalafx.scene.paint._
import scalafx.event._
import scalafx.animation._
import scalafx.scene.canvas._
import scalafx.scene.control.Label
import scalafx.scene.layout.{Background, BorderPane}

import scala.collection.mutable.Buffer

object testApp extends JFXApp {

  val space = new Space

  stage = new JFXApp.PrimaryStage {

    title = "testApp"
    scene = new Scene(1700,900) {
      val canvas = new Canvas(1700,900)
      val gc = canvas.graphicsContext2D
      val border = new BorderPane
      border.center = canvas
      root = border
      fill = Color.rgb(140,140,140)
      content = List()


      val timer = AnimationTimer { time =>
        content.clear()
        for (body <- space.bodies) {
          var i = space.bodies.indexOf(body)
          content.add(i,Circle.apply(body.pos.x,body.pos.y,body.radius,Color.White))
          content.get(i).setEffect(new InnerShadow(body.radius,body.radius/5,body.radius/5,Color.Gray))
        }
        for (body <- space.bodies) {
          var nimi = new Text(body.pos.x-body.radius/2,body.pos.y-body.radius-40,body.name)
          content.add(nimi)
          var velo = new Text(body.pos.x-body.radius/2,body.pos.y-body.radius-25,"v: " + body.velocity.magnitude.toString)
          content.add(velo)
          var acc = new Text(body.pos.x-body.radius/2,body.pos.y-body.radius-10,"a: " + body.acceleration.magnitude.toString)
          content.add(acc)
        }
        for (body <- space.bodies) {
          var viiva = new Line()
          viiva.startX = body.pos.x-body.radius/5
          viiva.startY = body.pos.y+body.radius+10
          viiva.endX = body.acceleration.x*400 + body.pos.x-body.radius/5
          viiva.endY = body.acceleration.y*400 + body.pos.y+body.radius+10
          content.add(viiva)
        }


        space.advanceStepInTime()
      }
      timer.start()
      canvas.requestFocus()

      val star = new AstralBody(space, "Helmeri",900, 30, new Vector3((canvas.width/2).toDouble, (canvas.height/2).toDouble, 0), new Vector3(0,0,0))
      val planet = new AstralBody(space, "Latios", 10, 10, new Vector3(500,450,0), new Vector3(0,-1.5,0))
      val p = new AstralBody(space, "Pikku Myy", 7, 10, new Vector3(1300,450,0), new Vector3(0,1.5,0))
      space.addBody(star)
      space.addBody(planet)
      space.addBody(p)
      space.setTimeStep(1)

    }
  }
}


