
import scala.math._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.control._
import scalafx.scene.shape._
import scalafx.scene.text._
import scalafx.scene.effect._
import scalafx.scene.paint._
import scalafx.event._
import scalafx.animation._
import scalafx.scene.canvas._
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout._


object testApp extends JFXApp {

  /** Function for rounding doubles */
  def roundAt(p: Int)(n: Double): Double = {
    val s = math.pow(10, p)
    (math round n * s) / s
  }

  /** Function for taking x,y,z -values from "x, y, z" */
  def takeValues(t: String): Vector3 = {
    var txt = t
    val x = txt.takeWhile(_!=',')
    txt = txt.drop(x.length+2)
    val y = txt.takeWhile(_!=',')
    txt = txt.drop(y.length+2)
    val z = txt
    new Vector3(x.toDouble, y.toDouble, z.toDouble)
  }

  /** Initializing space */
  val space = new Space

  /*val star = new AstralBody(space, "Helmeri",900E10, 30, new Vector3(0, 0, 0), new Vector3(0,0,0))
  val planet = new AstralBody(space, "Latios", 10, 10, new Vector3(-400,-40,200), new Vector3(0,0.5,0.5))
  val p = new AstralBody(space, "Pikku Myy", 15, 10, new Vector3(500,0,-200), new Vector3(0,0.5,0))
  space.addBody(star)
  space.addBody(planet)
  space.addBody(p)*/
  val earth = new AstralBody(space, "Maa", 5.97E24, 8, new Vector3(149.6E9,0,0), new Vector3(0,29.78E3,0))
  val sun = new AstralBody(space, "Aurinko", 1.989E30, 25, new Vector3(0,0,0), new Vector3(0,0,0))
  val moon = new AstralBody(space, "Kuu", 7.34E22, 4, new Vector3(149.6E9+384.4E6, 0, 0), new Vector3(0,29.78E3+1.022E3,0))
  space.addBody(earth)
  space.addBody(sun)
  space.addBody(moon)

  space.setTimeStep(400)

  class Arrow(startX: Double, startY: Double, endX: Double, endY: Double, arrowSize: Double) extends Path {
    this.strokeProperty().bind(fill)
    this.setFill(Color.Red)

    val moveTo = new MoveTo()
    moveTo.setX(startX)
    moveTo.setY(startY)
    this.getElements.add(moveTo)
    val lineTo = new LineTo()
    lineTo.setX(endX)
    lineTo.setY(endY)
    this.getElements.add(lineTo)

    val angle = atan2((endY-startY),(endX-startX)) - math.Pi/2
    val s = sin(angle)
    val c = cos(angle)

    val x1 = (-1.0/2.0 * c + sqrt(3)/2 * s) * arrowSize + endX
    val y1 = (-1.0/2.0 * s - sqrt(3)/2 * c) * arrowSize + endY
    val x2 = (1.0/2.0 * c + sqrt(3)/2 * s) * arrowSize + endX
    val y2 = (1.0/2.0 * s - sqrt(3)/2 * c) * arrowSize + endY

    val aLine1 = new LineTo()
    aLine1.setX(x1)
    aLine1.setY(y1)
    val aLine2 = new LineTo()
    aLine2.setX(x2)
    aLine2.setY(y2)
    val aLine3 = new LineTo()
    aLine3.setX(endX)
    aLine3.setY(endY)
    this.getElements.add(aLine1)
    this.getElements.add(aLine2)
    this.getElements.add(aLine3)

  }


  stage = new JFXApp.PrimaryStage {


    title = "testApp"
    scene = new Scene(1700,900) {

      content = List()

      val border = new BorderPane

      val tabPane = new TabPane

      var offSetX: Long = 800
      var offSetY: Long = 450
      var zoomLevel: Double = 1E9
      var centerX: Long = 800
      var centerY: Long = 450

      /** Creates the Tab for data, which includes necessary information of
       *  the bodies in space */
      val dataTab = new Tab
      dataTab.text = "Data"

      val dataAccordion = new Accordion
      dataAccordion.setMaxHeight(100)
      dataAccordion.setPrefWidth(200)
      for (b <- space.bodies) {
        val titledPane = new TitledPane
        titledPane.text = b.name
        val vbox = new VBox
        vbox.setSpacing(10)
        vbox.setPrefWidth(200)
        vbox.children = List(new Text("mass: " + b.mass + " kg"),
          new Text("position: " + "(" + b.pos.x.round+ ", " + b.pos.y.round + ", " + b.pos.z.round +")"),
          new Text("acceleration: " + roundAt(7)(b.acceleration.magnitude) + " m/s^2"),
          new Text("velocity: " + roundAt(7)(b.velocity.magnitude) + " m/s"))
        titledPane.content = vbox

        dataAccordion.panes.add(titledPane)
      }
      val dataScroll = new ScrollPane
      dataScroll.content = dataAccordion
      dataTab.content = dataScroll

      val viewTab = new Tab
      viewTab.text = "View"
      viewTab.closable = false
      val pauseButton = new Button("Pause")
      pauseButton.setLayoutX(5)
      pauseButton.setLayoutY(5)

      pauseButton.onAction = (e: ActionEvent) => {
        pauseButton.getText match {
          case "Pause" => { timer.stop(); pauseButton.text = "Play" }
          case "Play" => { timer.start(); pauseButton.text = "Pause"}
        }
      }


      /** Creates a Tab for adding new bodies into the space */

      val bodyTab = new Tab
      bodyTab.text = "Add body"

      /** Button for adding a new body */
      var adder = new Button("Add Body")

      adder.onAction = (event: ActionEvent) =>  {
        posField.getText.takeWhile(_!=',')
        var uusi = new AstralBody(space, nameField.getText, massField.getText.toDouble, radiusField.getText.toDouble,
          takeValues(posField.getText), takeValues(veloField.getText))
        space.addBody(uusi)
        /** Adds the body into the scene */
        val pallo = Circle.apply(uusi.radius,Color.MistyRose)
        pallo.setEffect(new InnerShadow(uusi.radius, -uusi.radius/2, -uusi.radius/2, Color.Black))
        pallo.setLayoutX(uusi.pos.x)
        pallo.setLayoutY(uusi.pos.y)
        pallo.setId(uusi.name)
        bodyPane.children.add(pallo)
        /** Adds the name-text */
        var name = new Text(uusi.name)
        name.fill = Color.White
        name.setId(uusi.name)
        textPane.children.add(name)
        val titledPane = new TitledPane
        titledPane.text = uusi.name
        dataAccordion.panes.add(titledPane)
      }

      val bodyVBox = new VBox
      bodyVBox.setSpacing(10)
      var nameField = new TextField
      var massField = new TextField
      var radiusField = new TextField
      var posField = new TextField
      var veloField = new TextField
      bodyVBox.children = List(new Text("Name:"), nameField, new Text("Mass:  (kg)"), massField, new Text("Radius:  (pixels)"), radiusField,
        new Text("Position:  x, y, z  (m)"), posField, new Text("Velocity:  x, y, z   (m/s)"), veloField, adder)

      bodyTab.content = bodyVBox

      dataTab.closable = false
      bodyTab.closable = false


      /** Checkbox for showing the names of the bodies */
      val showNames = new CheckBox("Show names")
      showNames.selected = true
      showNames.onAction = (e: ActionEvent) => {
        if (showNames.isSelected) pane.children.add(textPane)
        else pane.children.remove(textPane)
      }
      /** Checkbox for showing the vectors of the bodies */
      val showAccVector = new CheckBox("Show acceleration vector")
      showAccVector.onAction = (e: ActionEvent) => {
        if (showAccVector.isSelected) pane.children.add(accVectorPane)
        else pane.children.remove(accVectorPane)
      }
      val showVelVector = new CheckBox("Show velocity vector")
      showVelVector.onAction = (e: ActionEvent) => {
        if (showVelVector.isSelected) pane.children.add(velVectorPane)
        else pane.children.remove(velVectorPane)
      }

      val viewVBox = new VBox
      viewVBox.setSpacing(10)
      viewVBox.children = List(pauseButton, showNames, showAccVector, showVelVector)

      viewTab.content = viewVBox


      /** Creates the pane containing the graphical view of the space */
      val pane = new Pane
      pane.setStyle("-fx-background-color: #1C1C1C;")


      /** Adds all bodies into a child pane of the graphical pane */
      val bodyPane = new Pane
      for (body <- space.bodies) {
        val pallo = Circle.apply(body.radius,Color.MistyRose)
        pallo.setEffect(new InnerShadow(body.radius, -body.radius/3, -body.radius/3, Color.Black))
        pallo.setLayoutX(body.pos.x+offSetX)
        pallo.setLayoutY(body.pos.y+offSetY)
        pallo.setId(body.name)
        bodyPane.children.add(pallo)
      }
      /** Adds names into a child pane of the graphical pane */
      val textPane = new Pane
      for (body <- space.bodies) {
        var name = new Text(body.name)
        name.fill = Color.White
        name.setId(body.name)
        textPane.children.add(name)
      }
      /** Pane for vectors */
      val accVectorPane = new Pane
      for (body <- space.bodies) {
        val acc = new Arrow(body.pos.x,body.pos.y,0,0,0)
        acc.setId(body.name)
        accVectorPane.children.add(acc)
      }
      val velVectorPane = new Pane
      for (body <- space.bodies) {
        val v = new Arrow(body.pos.x,body.pos.y,0,0,0)
        v.setId(body.name)
        velVectorPane.children.add(v)
      }

      pane.children = List(bodyPane, textPane)
      tabPane.tabs = List(dataTab, viewTab, bodyTab)
      // closingPolicy
      border.center = pane
      border.left = tabPane

      root = border

      //val focuser =

      pane.onMouseClicked = (e: MouseEvent) => {
        pane.requestFocus()
      }
      pane.onKeyPressed = (e: KeyEvent) => {
        if (e.code == KeyCode.W) offSetY += 30
        else if (e.code == KeyCode.S) offSetY -= 30
        else if (e.code == KeyCode.A) offSetX += 30
        else if (e.code == KeyCode.D) offSetX -= 30
        else if (e.code == KeyCode.Up) zoomLevel = zoomLevel/2
        else if (e.code == KeyCode.Down) zoomLevel = zoomLevel/2
        println(offSetX)
      }


      /** AnimationTimer for controlling the changes. Timer ticks at 60 Hz */

      val timer = AnimationTimer { time =>



        /** Sorts the bodies based on their position,
         *  so that a body further on the z-axis will be below a body that has a lower z-axis */
        val sortedBodies = bodyPane.children.sortBy(n => space.bodies.filter(_.name == n.getId).head.pos.z).toList.reverse
        bodyPane.children.clear()
        sortedBodies.foreach(bodyPane.children.add(_))

        /** Moves the bodies, their names and vectors */
        for (c <- bodyPane.children) {
          val b = space.bodies.filter(_.name == c.getId).head
          c.setLayoutX(b.pos.x/zoomLevel+offSetX)
          c.setLayoutY(b.pos.y/zoomLevel+offSetY)
        }
        for (t <- textPane.children) {
          val b = space.bodies.filter(_.name == t.getId).head
          t.setLayoutX(b.pos.x/zoomLevel+offSetX)
          t.setLayoutY(-7-b.radius+b.pos.y/zoomLevel+offSetY)
        }
        accVectorPane.children.clear()
        for (b <- space.bodies) {
          val endX = b.pos.x/zoomLevel + b.direction.x*b.acceleration.magnitude*4000
          val endY = b.pos.y/zoomLevel + b.direction.y*b.acceleration.magnitude*4000
          val arrow = new Arrow(b.pos.x/zoomLevel+offSetX, b.pos.y/zoomLevel+offSetY, endX+offSetX, endY+offSetY, 5)
          accVectorPane.children.add(arrow)
        }
        velVectorPane.children.clear()
        for (b <- space.bodies) {
          val endX = b.pos.x/zoomLevel + b.velocityDirection.x*b.velocity.magnitude/1000
          val endY = b.pos.y/zoomLevel + b.velocityDirection.y*b.velocity.magnitude/1000
          val arrow = new Arrow(b.pos.x/zoomLevel+offSetX, b.pos.y/zoomLevel+offSetY, endX+offSetX, endY+offSetY, 5)
          arrow.setFill(Color.Green)
          velVectorPane.children.add(arrow)
        }


        /** Updates the data in the data tab */
        for (b <- space.bodies) {
          val vbox = new VBox
          vbox.setSpacing(10)
          vbox.setPrefWidth(200)
          vbox.children = List(new Text("mass: " + b.mass + " kg"),
            new Text("position: " + "(" + b.pos.x.round+ ", " + b.pos.y.round + ", " + b.pos.z.round +")"),
            new Text("acceleration: " + roundAt(7)(b.acceleration.magnitude) + " m/s^2"),
            new Text("velocity: " + roundAt(7)(b.velocity.magnitude) + " m/s"))
          dataAccordion.panes.get(space.bodies.indexOf(b)).content = vbox
        }


        space.advanceStepInTime()




      }
      timer.start()


    }
  }
  // stage.setMaximized(true)
}


