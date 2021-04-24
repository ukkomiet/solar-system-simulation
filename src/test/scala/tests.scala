object tests extends App {
  val space = new Space

  val planetA = new AstralBody(space, "Helmeri",10, 20, new Vector3(0, 0, 0), new Vector3(0,0,0))
  val planetB = new AstralBody(space, "Santeri",10, 20, new Vector3(100, 0, 0), new Vector3(0,0,0))
  val planetC = new AstralBody(space, "Ellus",10, 20, new Vector3(-100, 0, 0), new Vector3(0,0,0))
  space.setTimeStep(10)
  space.addBody(planetA)
  space.addBody(planetB)
  space.addBody(planetC)
  println("tulokset.")
  space.advanceStepInTime()
  println("a of B: " + planetB.acceleration.x)
  println("v of B: " + planetB.velocity.x)
  space.advanceStepInTime()
  println(planetA.pos.x)
  println(planetC.pos.x)
}
