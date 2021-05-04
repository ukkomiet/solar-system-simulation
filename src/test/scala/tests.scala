


object tests extends App {
  val space = new Space

  val planetA = new AstralBody(space, "Helmeri",150E18, 20, new Vector3(0, 0, 0), new Vector3(0,0,0))
  val planetB = new AstralBody(space, "Santeri",20, 20, new Vector3(-100, 2000, -100), new Vector3(0,0,0))
  val planetC = new AstralBody(space, "Ellus",3000000, 20, new Vector3(5000, 2000, 100), new Vector3(0,0,0))
  space.setTimeStep(1)
  space.addBody(planetA)
  space.addBody(planetB)
  space.addBody(planetC)
  space.updateGravitiesForBodies()
  println("tulokset.")
  space.advanceStepInTime()
  println(planetB.pos.x)
  space.advanceStepInTime()
  println(planetB.pos.x)
  space.advanceStepInTime()
  println(planetB.pos.x)

}
