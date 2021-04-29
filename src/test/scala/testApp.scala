
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
import scalafx.scene.layout._

import scala.collection.mutable.Buffer

object testApp extends JFXApp {

  /** Function for rounding doubles */
  def roundAt(p: Int)(n: Double): Double = {
    val s = math.pow(10, p)
    (math round n * s) / s
  }

  val space = new Space

  val star = new AstralBody(space, "Helmeri",900E10, 30, new Vector3(0, 0, 0), new Vector3(0,0,0))
  val planet = new AstralBody(space, "Latios", 10, 10, new Vector3(-300,0,100), new Vector3(0,-1.5,0))
  val p = new AstralBody(space, "Pikku Myy", 7, 10, new Vector3(500,0,-200), new Vector3(0,1.5,0))
  space.addBody(star)
  space.addBody(planet)
  space.addBody(p)
  space.setTimeStep(1)


  stage = new JFXApp.PrimaryStage {

    title = "testApp"
    scene = new Scene(1700,900) {
      content = List()
      val canvas = new Canvas(1700,900)
      val gc = canvas.graphicsContext2D

      val border = new BorderPane

      val tabPane = new TabPane



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
      val nap = new TextField
      viewTab.content = nap

      /** Creates a Tab for adding new bodies into the space */

      val bodyTab = new Tab
      bodyTab.text = "Add body"

      /** Button for adding a new body */
      var adder = new Button("Add Body")

      adder.onAction = (event: ActionEvent) =>  {
        var uusi = new AstralBody(space, nameField.getText, massField.getText.toDouble, radiusField.getText.toDouble,
          new Vector3(-300,0,100), new Vector3(0,0,0))
        space.addBody(uusi)
        val pallo = Circle.apply(uusi.radius,Color.MistyRose)
        pallo.setEffect(new InnerShadow(uusi.radius, -uusi.radius/2, -uusi.radius/2, Color.Black))
        pallo.setLayoutX(uusi.pos.x)
        pallo.setLayoutY(uusi.pos.y)
        pane.children.add(space.bodies.indexOf(uusi), pallo)
        var i = space.bodies.length
        var name = new Text(uusi.name)
        name.fill = Color.White
        pane.children.add(i+space.bodies.indexOf(uusi), name)
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
      bodyVBox.children = List(new Text("Name:"), nameField, new Text("Mass:"), massField, new Text("Radius:"), radiusField,
        new Text("Position:"), posField, new Text("Velocity:"), veloField, adder)

      bodyTab.content = bodyVBox



      dataTab.closable = false
      bodyTab.closable = false






      /** Creates the pane containing the graphical view of the space */
      val pane = new Pane
      pane.setStyle("-fx-background-color: #3C3C3C;")

      /** Adds all the bodies into the pane */
      for (body <- space.bodies) {
        val pallo = Circle.apply(body.radius,Color.MistyRose)
        pallo.setEffect(new InnerShadow(body.radius, -body.radius/2, -body.radius/2, Color.Black))
        pallo.setLayoutX(body.pos.x+800)
        pallo.setLayoutY(body.pos.y+450)
        pane.children.add(space.bodies.indexOf(body), pallo)
      }
      for (body <- space.bodies) {
        var i = space.bodies.length
        var name = new Text(body.name)
        name.fill = Color.White
        pane.children.add(i+space.bodies.indexOf(body), name)
      }

      tabPane.tabs = List(dataTab, viewTab, bodyTab)
      border.center = pane
      border.left = tabPane

      root = border






      val timer = AnimationTimer { time =>


        /** Moves the bodies and their respective names */
        for (i <- space.bodies.indices) {
          pane.children.get(i).setLayoutX(space.bodies(i).pos.x+800)
          pane.children.get(i).setLayoutY(space.bodies(i).pos.y+450)
        }
        for (i <- space.bodies.indices) {
          pane.children.get(i+space.bodies.length).setLayoutX(space.bodies(i).pos.x+800)
          pane.children.get(i+space.bodies.length).setLayoutY(space.bodies(i).pos.y-space.bodies(i).radius-10+450)
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
}


