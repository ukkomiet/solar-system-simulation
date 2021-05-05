
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
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout._



object App extends JFXApp {

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

  val earth = new AstralBody(space, "Earth", 5.97E24, 6371E3,5, new Vector3(149.6E9,0,0), new Vector3(0,29.78E3,0))
  val sun = new AstralBody(space, "Sun", 1.989E30, 696340E3, 20, new Vector3(0,0,0), new Vector3(0,0,0))
  //val moon = new AstralBody(space, "Moon", 7.34E22, 1737.1E3, 3, new Vector3(149.6E9+384.4E6, 0, 0), new Vector3(0,29.78E3+1.022E3,0))
  val venus = new AstralBody(space, "Venus", 4.86E24, 6051.8E3, 5, new Vector3(108E9,0,0), new Vector3(0,35E3,0))
  val mercury = new AstralBody(space, "Mercury", 0.33E24, 2439.7E3, 2, new Vector3(57.91E9,0,0), new Vector3(0,47E3,0))
  val mars = new AstralBody(space, "Mars", 0.642E24, 3389.5E3, 4, new Vector3(227.92E9,0,0), new Vector3(0,24E3,0))
  val jupiter = new AstralBody(space, "Jupiter", 1898E24, 69911E3, 13, new Vector3(0,778.5E9,0), new Vector3(-13.07E3,0,0))
  val saturn = new AstralBody(space, "Saturn", 568E24, 58232E3, 10, new Vector3(1433.5E9,0,0), new Vector3(0,9.7E3,0))
  val uranus = new AstralBody(space, "Uranus", 86.8E24, 25362E3, 8, new Vector3(2872.5E9,0,0), new Vector3(0,6.8E3,0))
  val neptune = new AstralBody(space, "Neptune", 102E24, 24622E3, 7, new Vector3(4495.1E9,0,0), new Vector3(0,5.4E3,0))
  val pluto = new AstralBody(space, "Pluto", 0.0146E24, 1188.3E3, 2, new Vector3(5906E9,0,0), new Vector3(0,4.7E3,0))
  space.addBody(sun)
  space.addBody(mercury)
  space.addBody(venus)
  space.addBody(earth)
  space.addBody(mars)
  space.addBody(jupiter)
  space.addBody(saturn)
  space.addBody(uranus)
  space.addBody(neptune)
  space.addBody(pluto)
  //space.addBody(moon)

  space.setTimeStep(10000)

  /** Class for an arrow shape */
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

      /** Panning and zooming variables */
      var offSetX: Long = 800
      var offSetY: Long = 450
      var pan: Long = 15
      var zoomLevel: Double = 1E9
      val init_zoomLvl: Double = 1E9
      val centerX: Long = 800
      val centerY: Long = 450

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
          new Text("d from Sun: " + round(10*(space.distanceBetweenBodies(b, sun)))/10 + " m"),
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

      /** Button for pausing the simulation */
      pauseButton.onAction = (e: ActionEvent) => {
        pauseButton.getText match {
          case "Pause" => { timer.stop(); pauseButton.text = "Play" }
          case "Play" => { timer.start(); pauseButton.text = "Pause"}
        }
      }


      /** Creates a Tab for adding new bodies into the space */
      val bodyTab = new Tab
      bodyTab.text = "Add/Launch"

      /** Button for adding a new body */
      var adder = new Button("Add Body")

      adder.onAction = (event: ActionEvent) =>  {

        val distance = dField.getText.toDouble //   x/zoomLevel+offSetX
        var dir = takeValues(directionField.getText).normalize
        dir.x = dir.x*distance
        dir.y = dir.y*distance
        dir.z = dir.z*distance
        val pos = sun.pos.+(dir)

        val uusi = new AstralBody(space, nameField.getText, massField.getText.toDouble, radiusField.getText.toDouble, drawRadiusField.getText.toLong,
          pos, takeValues(veloField.getText))
        uusi.isSatellite = false
        uusi.isNotSatellite = true
        space.addBody(uusi)
        /** Adds the body into the scene */
        val pallo = Circle.apply(uusi.drawRadius,Color.MistyRose)
        pallo.setEffect(new InnerShadow(uusi.drawRadius, -uusi.drawRadius/2, -uusi.drawRadius/2, Color.Black))
        pallo.setLayoutX(uusi.pos.x)
        pallo.setLayoutY(uusi.pos.y)
        pallo.setId(uusi.name)
        bodyPane.children.add(pallo)
        /** Adds the name-text */
        val name = new Text(uusi.name)
        name.fill = Color.White
        name.setId(uusi.name)
        textPane.children.add(name)
        val titledPane = new TitledPane
        titledPane.text = uusi.name
        val vbox = new VBox
        vbox.setSpacing(10)
        vbox.setPrefWidth(200)
        vbox.children = List(new Text("mass: " + uusi.mass + " kg"),
          new Text("d from Sun: " + round(10*(space.distanceBetweenBodies(uusi, sun)))/10 + " m"),
          new Text("acceleration: " + roundAt(7)(uusi.acceleration.magnitude) + " m/s^2"),
          new Text("velocity: " + roundAt(7)(uusi.velocity.magnitude) + " m/s"))
        titledPane.content = vbox

        dataAccordion.panes.add(titledPane)
      }

      /** Launching satellites
       *  Adds a small body that draws a graph while moving */
      val satelliteButton = new Button("Launch")
      val satelliteText = new Text("Launch satellite:")
      val sName = new TextField
      val satelliteName = new HBox(new Text("Name:"), sName)
      satelliteName.setSpacing(5)
      val sMass = new TextField
      val satelliteMass = new HBox(new Text("Mass: kg"), sMass)
      satelliteMass.setSpacing(5)
      val sFrom = new TextField
      val satelliteFrom = new HBox(new Text("Launch from:"), sFrom)
      satelliteFrom.setSpacing(5)
      val satelliteDirectionText = new Text("Direction:  x, y, z")
      val satelliteDirection = new TextField
      val sVel = new TextField
      val satelliteVelocity = new HBox(new Text("Velocity: m/s"), sVel)
      satelliteVelocity.setSpacing(5)


      /** Adds the satellite into bodies, adds the dot into bodyPane, adds the name into textPane
       *  and finally adds the satellite-data into the dataTab */
      satelliteButton.onAction = (e: ActionEvent) => {
        val direction = takeValues(satelliteDirection.getText).normalize

        var v = 0.0
        val name = sName.getText
        val mass = sMass.getText.toDouble
        if (sVel.getText.toDoubleOption.isDefined) {v =sVel.getText.toDouble}
        val velocity = new Vector3(direction.x*v, direction.y*v, direction.z*v)
        val from = space.bodies.filter(_.name == sFrom.getText).head
        val pos = from.pos
        val rect = new Rectangle
        rect.fill = Color.White
        rect.setWidth(3)
        rect.setHeight(3)
        rect.id = name
        rect.setLayoutX(pos.x/zoomLevel+offSetX)
        rect.setLayoutY(pos.y/zoomLevel+offSetY)
        val r = new Vector3(direction.x*from.radius, direction.y*from.radius, direction.z*from.radius)
        val uusi = new AstralBody(space, name, 123, 30, 3, r.+(pos), velocity)
        uusi.isSatellite = true
        uusi.isNotSatellite = false
        space.addBody(uusi)
        bodyPane.children.add(rect)

        /** Adds the name into the textPane */
        val nText = new Text(name)
        nText.id = name
        nText.fill = Color.White
        nText.layoutX = uusi.pos.x/zoomLevel+offSetX
        nText.layoutY = uusi.pos.y/zoomLevel+offSetY -uusi.drawRadius*init_zoomLvl/zoomLevel-3
        textPane.children.add(nText)

        /** Adds the satellites data into the data-tab */
        val titledPane = new TitledPane
        titledPane.text = "Satellite: " + name
        val vbox = new VBox
        vbox.setSpacing(10)
        vbox.setPrefWidth(200)
        vbox.children = List(new Text("mass: " + mass + " kg"),
          new Text("d from Sun: " + round(10*(space.distanceBetweenBodies(uusi, sun)))/10 + " m"),
          new Text("acceleration: " + roundAt(7)(uusi.acceleration.magnitude) + " m/s^2"),
          new Text("velocity: " + roundAt(7)(uusi.velocity.magnitude) + " m/s"))
        titledPane.content = vbox
        dataAccordion.panes.add(titledPane)
      }

      /** Tab for adding bodies and launching satellites */
      val bodyVBox = new VBox
      bodyVBox.setSpacing(10)
      bodyVBox.prefWidth = 100
      val nameField = new TextField
      val massField = new TextField
      val radiusField = new TextField
      val drawRadiusField = new TextField
      val dField = new TextField
      val directionField = new TextField
      val veloField = new TextField
      bodyVBox.children = List(new Text("Name:"), nameField, new Text("Mass:  (kg)"), massField, new Text("Radius:  (m)"), radiusField, new Text("Drawn radius:  (pixels)"),
        drawRadiusField, new Text("Distance from Sun:  (m)"), dField, new Text("Direction from Sun:  x, y, z"),
        directionField, new Text("Velocity:  x, y, z   (m/s)"), veloField, adder,
        satelliteText, satelliteName, satelliteMass, satelliteFrom, satelliteDirectionText, satelliteDirection, satelliteVelocity, satelliteButton)

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
      /** Button for changing the timeStep-variable */
      val timeStepper = new TextField
      val timeText = new Text("Change time step:")
      val timeButton = new Button("Set")
      timeButton.onAction = (e: ActionEvent) => {
        space.setTimeStep(timeStepper.getText.toDouble)
      }
      val timeHBox = new HBox
      timeHBox.children = List(timeStepper, timeButton)
      timeHBox.setSpacing(5)

      /** Reset-view button */
      val resetView = new Button("Reset view")
      resetView.onAction = (e: ActionEvent) => {
        offSetX = centerX
        offSetY = centerY
        zoomLevel = init_zoomLvl
        zoomText.text = "Zoom: " + zoomLevel.toString
          /** Changing the radius of the circles depending on zoomLevel
           *  When zooming into the picture, the radius of each body will rise */
          bodyPane.children.clear()
          for (body <- space.bodies.filter(_.isNotSatellite)) {
           val pallo = Circle.apply(body.drawRadius*init_zoomLvl/zoomLevel,Color.MistyRose)
           pallo.setEffect(new InnerShadow(body.drawRadius*init_zoomLvl/zoomLevel, -body.drawRadius*init_zoomLvl/zoomLevel/3, -body.drawRadius*init_zoomLvl/zoomLevel/3, Color.Black))
           pallo.setLayoutX(body.pos.x+offSetX)
           pallo.setLayoutY(body.pos.y+offSetY)
           pallo.setId(body.name)
           pallo.getId match {
             case "Sun" => pallo.fill = Color.Gold
             case "Earth" => pallo.fill = Color.LightBlue
             case "Mercury" => pallo.fill = Color.Beige
             case "Venus" => pallo.fill = Color.RosyBrown
             case "Mars" => pallo.fill = Color.RosyBrown
             case "Jupiter" => pallo.fill = Color.FloralWhite
             case "Saturn" => pallo.fill = Color.Burlywood
             case "Uranus" => pallo.fill = Color.SkyBlue
             case "Neptune" => pallo.fill = Color.DarkCyan
             case "Pluto" => pallo.fill = Color.BlanchedAlmond
             case _=> pallo.fill = Color.MistyRose
            }
          bodyPane.children.add(pallo)
          }
         for (satellite <- space.bodies.filter(_.isSatellite)) {
            val rect = Rectangle.apply(satellite.drawRadius, satellite.drawRadius, Color.White)
            rect.setLayoutX(satellite.pos.x+offSetX)
            rect.setLayoutY(satellite.pos.y+offSetY)
            rect.setId(satellite.name)
            bodyPane.children.add(rect)
          }
      }
      val resetTime = new Button("Reset time")
      resetTime.onAction = (e: ActionEvent) => {
        space.time = 0
        spaceTime.text = "Time: " + round(space.time/(3600*24)) + " days"
      }

      val pauseAndreset = new HBox
      pauseAndreset.setSpacing(10)
      pauseAndreset.children = List(pauseButton, resetView)

      /** Pan-level and zoom-level input-fields */
      val panText = new Text("Change pan-level: (pixels)")
      val panField = new TextField
      val panButton = new Button("Set")
      panButton.onAction = (e: ActionEvent) => {
        pan = panField.getText.toLong
        panlevelText.text = "Pan: " + pan.toString
      }
      val zoomlevelText = new Text("Change zoom-level: (m)")
      val zoomlevelField = new TextField
      val zoomlevelButton = new Button("Set")
      zoomlevelButton.onAction = (e: ActionEvent) => {
        zoomLevel = zoomlevelField.getText.toDouble
        zoomText.text = "Zoom: " + zoomLevel.toString
      }
      val panHBox = new HBox
      panHBox.setSpacing(5)
      panHBox.children = List(panField, panButton)
      val zoomHBox = new HBox
      zoomHBox.setSpacing(5)
      zoomHBox.children = List(zoomlevelField, zoomlevelButton)
      val viewVBox = new VBox
      viewVBox.setSpacing(10)
      viewVBox.children = List(pauseAndreset, resetTime, showNames, showAccVector, showVelVector, timeText, timeHBox, panText, panHBox,
        zoomlevelText, zoomHBox)

      viewTab.content = viewVBox


      /** Creates the pane containing the graphical view of the space */
      val pane = new Pane
      pane.setStyle("-fx-background-color: #1C1C1C;")


      /** Adds all bodies into a child pane of the graphical pane */
      val bodyPane = new Pane
      for (body <- space.bodies.filter(_.isNotSatellite)) {
        val pallo = Circle.apply(body.drawRadius,Color.MistyRose)
        pallo.setEffect(new InnerShadow(body.drawRadius, -body.drawRadius/3, -body.drawRadius/3, Color.Black))
        pallo.setLayoutX(body.pos.x+offSetX)
        pallo.setLayoutY(body.pos.y+offSetY)
        pallo.setId(body.name)
        pallo.getId match {
          case "Sun" => pallo.fill = Color.Gold
          case "Earth" => pallo.fill = Color.LightBlue
          case "Mercury" => pallo.fill = Color.Beige
          case "Venus" => pallo.fill = Color.RosyBrown
          case "Mars" => pallo.fill = Color.RosyBrown
          case "Jupiter" => pallo.fill = Color.FloralWhite
          case "Saturn" => pallo.fill = Color.Burlywood
          case "Uranus" => pallo.fill = Color.SkyBlue
          case "Neptune" => pallo.fill = Color.DarkCyan
          case "Pluto" => pallo.fill = Color.BlanchedAlmond
          case _=> pallo.fill = Color.MistyRose
        }
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

      /** Top left data */
      val zoomText = new Text("Zoom: " + zoomLevel)
      zoomText.fill = Color.White
      val panlevelText = new Text("Pan: " + pan)
      panlevelText.fill = Color.White
      val topleftVBox = new VBox
      topleftVBox.setSpacing(5)
      topleftVBox.setLayoutX(3)
      topleftVBox.setLayoutY(2)
      val spaceTime = new Text("Time: " + round(space.time/(3600*24)) + " days")
      spaceTime.fill = Color.White
      topleftVBox.children = List(zoomText, panlevelText, spaceTime)

      pane.children = List(bodyPane, textPane, topleftVBox)
      tabPane.tabs = List(dataTab, viewTab, bodyTab)
      border.center = pane
      border.left = tabPane



      root = border



      /** Panning and zooming */
      pane.onMouseClicked = (e: MouseEvent) => {
        pane.requestFocus()
      }
      pane.onKeyPressed = (e: KeyEvent) => {
        if (e.code == KeyCode.W) offSetY += pan
        else if (e.code == KeyCode.S) offSetY -= pan
        else if (e.code == KeyCode.A) {offSetX += pan}
        else if (e.code == KeyCode.D) {offSetX -= pan}
        else if (e.code == KeyCode.Up) {
          zoomLevel = zoomLevel/2
          zoomText.text = "Zoom: " + zoomLevel.toString
          /** Changing the radius of the circles depending on zoomLevel
           *  When zooming into the picture, the radius of each body will rise */
          bodyPane.children.clear()
          for (body <- space.bodies.filter(_.isNotSatellite)) {
           val pallo = Circle.apply(body.drawRadius*init_zoomLvl/zoomLevel,Color.MistyRose)
           pallo.setEffect(new InnerShadow(body.drawRadius*init_zoomLvl/zoomLevel, -body.drawRadius*init_zoomLvl/zoomLevel/3, -body.drawRadius*init_zoomLvl/zoomLevel/3, Color.Black))
           pallo.setLayoutX(body.pos.x+offSetX)
           pallo.setLayoutY(body.pos.y+offSetY)
           pallo.setId(body.name)
           pallo.getId match {
             case "Sun" => pallo.fill = Color.Gold
             case "Earth" => pallo.fill = Color.LightBlue
             case "Mercury" => pallo.fill = Color.Beige
             case "Venus" => pallo.fill = Color.RosyBrown
             case "Mars" => pallo.fill = Color.RosyBrown
             case "Jupiter" => pallo.fill = Color.FloralWhite
             case "Saturn" => pallo.fill = Color.Burlywood
             case "Uranus" => pallo.fill = Color.SkyBlue
             case "Neptune" => pallo.fill = Color.DarkCyan
             case "Pluto" => pallo.fill = Color.BlanchedAlmond
             case _=> pallo.fill = Color.MistyRose
            }
          bodyPane.children.add(pallo)
          }
           for (satellite <- space.bodies.filter(_.isSatellite)) {
            val rect = Rectangle.apply(satellite.drawRadius, satellite.drawRadius, Color.White)
            rect.setLayoutX(satellite.pos.x+offSetX)
            rect.setLayoutY(satellite.pos.y+offSetY)
            rect.setId(satellite.name)
            bodyPane.children.add(rect)
          }


        }
        else if (e.code == KeyCode.Down) {
          zoomLevel = zoomLevel*2
          zoomText.text = "Zoom: " + zoomLevel.toString

          /** Changing the radius of the circles depending on zoomLevel.
           *  When zooming out of the picture, the radius of each body will lower */
          bodyPane.children.clear()
          for (body <- space.bodies.filter(_.isNotSatellite)) {
           val pallo = Circle.apply(body.drawRadius*init_zoomLvl/zoomLevel,Color.MistyRose)
           pallo.setEffect(new InnerShadow(body.drawRadius*init_zoomLvl/zoomLevel, -body.drawRadius*init_zoomLvl/zoomLevel/3, -body.drawRadius*init_zoomLvl/zoomLevel/3, Color.Black))
           pallo.setLayoutX(body.pos.x+offSetX)
           pallo.setLayoutY(body.pos.y+offSetY)
           pallo.setId(body.name)
           pallo.getId match {
             case "Sun" => pallo.fill = Color.Gold
             case "Earth" => pallo.fill = Color.LightBlue
             case "Mercury" => pallo.fill = Color.Beige
             case "Venus" => pallo.fill = Color.RosyBrown
             case "Mars" => pallo.fill = Color.RosyBrown
             case "Jupiter" => pallo.fill = Color.FloralWhite
             case "Saturn" => pallo.fill = Color.Burlywood
             case "Uranus" => pallo.fill = Color.SkyBlue
             case "Neptune" => pallo.fill = Color.DarkCyan
             case "Pluto" => pallo.fill = Color.BlanchedAlmond
             case _=> pallo.fill = Color.MistyRose
            }
          bodyPane.children.add(pallo)
          }
          for (satellite <- space.bodies.filter(_.isSatellite)) {
            val rect = Rectangle.apply(satellite.drawRadius, satellite.drawRadius, Color.White)
            rect.setLayoutX(satellite.pos.x+offSetX)
            rect.setLayoutY(satellite.pos.y+offSetY)
            rect.setId(satellite.name)
            bodyPane.children.add(rect)
          }

        }
      }



      /** AnimationTimer for controlling the changes.*/

      val timer = AnimationTimer { time =>

        space.advanceStepInTime()

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
          t.setLayoutY(b.pos.y/zoomLevel+offSetY -b.drawRadius*init_zoomLvl/zoomLevel-3)
        }
        accVectorPane.children.clear()
        for (b <- space.bodies) {
          val endX = b.pos.x/zoomLevel + b.direction.x*b.acceleration.magnitude*2000
          val endY = b.pos.y/zoomLevel + b.direction.y*b.acceleration.magnitude*2000
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
          val d = space.distanceBetweenBodies(b,sun)
          val vbox = new VBox
          vbox.setSpacing(10)
          vbox.setPrefWidth(200)
          vbox.children = List(new Text("mass: " + b.mass + " kg"),
            new Text("d from Sun: " + round(10*(space.distanceBetweenBodies(b, sun)))/10 + " m"),
            new Text("acceleration: " + roundAt(7)(b.acceleration.magnitude) + " m/s^2"),
            new Text("velocity: " + roundAt(7)(b.velocity.magnitude) + " m/s"))
          dataAccordion.panes.get(space.bodies.indexOf(b)).content = vbox
        }

        spaceTime.text = "Time: " + round(space.time/(3600*24)) + " days"






      }// Here ends animationTimer
      timer.start()


    }// Here ends scene
  }//Here ends stage
}//Here ends App


