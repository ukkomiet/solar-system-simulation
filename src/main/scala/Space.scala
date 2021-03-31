


import scala.math._

class Space {

  /** Contains all the bodies in the space */
  var bodies = Vector[AstralBody]()
  /** Keeps track of time */
  var time: Long = 0
  /** Current timeStep set by user, this is how much time is added to 'time' after each step */
  var timeStep = 0

  val G: Double = 0.0000000000667384

  /** Method for adding a body into the space */
  def addBody(b: AstralBody) = this.bodies :+ b

  /** Method for setting the time step */
  def setTimeStep(t: Int) = timeStep = t

  /** Calculates the direction vector between two points, from A to B */
  def directionAtoB(a: Vector3, b: Vector3) = {
    var res = new Vector3(0,0,0)
    res.x = b.x - a.x
    res.y = b.y - a.y
    res.z = b.z - a.z
    res.normalize
  }

  /** Method for calculating the distance between two bodies */
  def distanceBetweenBodies(a: AstralBody, b: AstralBody): Double = {
    val x = pow(a.pos.x-b.pos.x, 2)
    val y = pow(a.pos.y-b.pos.y, 2)
    val z = pow(a.pos.z-b.pos.z, 2)
    sqrt(x + y + z)
  }

  /** Calculates the gravity-force between Body B to Body A, returns it as a
   *  Vector3 with the correct magnitude (magnitude equals the force) and direction */
  def gravityVectorFromAtoB(a: AstralBody, b: AstralBody): Vector3 = {
    val force = G*(a.mass*b.mass)/(pow(distanceBetweenBodies(a,b), 2))
    var res = this.directionAtoB(a.pos, b.pos)
    res.x = res.x*force
    res.y = res.y*force
    res.z = res.z*force
    res
  }

  /** Updates the gravities in each body's allGravities-array */
  def updateGravitiesForBodies() = {

    for (body <- bodies) {
      body.allGravities = Vector[Vector3]()       // empties the body's list of earlier gravities
      val others = bodies.takeWhile(_ != body)    // creates a vector consisting of all the other bodies

      for (other <- others) {
        body.allGravities :+ gravityVectorFromAtoB(body, other)
      }
    }
  }


}