


object tests extends App {
  val space = new Space

  val planetA = new AstralBody(space, "Helmeri",15000000, 20, new Vector3(0, 0, 0), new Vector3(0,0,0))
  val planetB = new AstralBody(space, "Santeri",2000000, 20, new Vector3(-10000, 2000, -100), new Vector3(0,0,0))
  val planetC = new AstralBody(space, "Ellus",3000000, 20, new Vector3(5000, 2000, 100), new Vector3(0,0,0))
  space.setTimeStep(1)
  space.addBody(planetA)
  space.addBody(planetB)
  space.addBody(planetC)
  space.updateGravitiesForBodies()
  println("tulokset.")
  space.advanceStepInTime()
  println(planetA.pos.z)

}
