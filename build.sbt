name := "aurinkokunta"

version := "0.1"

scalaVersion := "2.13.5"


libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19"
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "14.0.1" classifier osName
)


//libraryDependencies += "org.processing" % "core" % "3.5.4"



resolvers += "Jzy3d releases" at "https://maven.jzy3d.org/releases/"

libraryDependencies ++= Seq(
      "org.jzy3d" % "jzy3d-api" % "1.0.2",
      "org.jzy3d" % "jzy3d-depthpeeling" % "1.0.2",
      "org.jzy3d" % "jzy3d-javafx" % "1.0.2"
)

