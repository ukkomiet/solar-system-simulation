


import scala.math._

class Space {

  /** Contains all the bodies in the space */
  var bodies = Vector[AstralBody]()

  /** Keeps track of time in seconds */
  var time: Double = 0                        // Unit: s

  /** Current timeStep set by user, this is how much time is added to 'time' after each step. The unit
   *  of a step is a second */
  var timeStep: Double = 0                          // Unit: s

  /** Holds the current Newtonian gravity-constant */
  val G: Double = 0.0000000000667384     // 0.0000000000667384, Unit: Nm^2/kg^2

  /** Method for adding a body into the space */
  def addBody(b: AstralBody) = this.bodies = this.bodies :+ b

  /** Method for setting the time step */
  def setTimeStep(t: Double) = timeStep = t

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
    val x = (a.pos.x-b.pos.x)*(a.pos.x-b.pos.x)
    val y = (a.pos.y-b.pos.y)*(a.pos.y-b.pos.y)
    val z = (a.pos.z-b.pos.z)*(a.pos.z-b.pos.z)
    sqrt(x + y + z)
  }

  /** Calculates the gravity-force between Body B to Body A, returns it as a
   *  Vector3 with the correct magnitude (magnitude equals the force) and direction */
  def gravityVectorFromAtoB(a: AstralBody, b: AstralBody): Vector3 = {
    val force = G*(a.mass*b.mass)/(distanceBetweenBodies(a,b)*distanceBetweenBodies(a,b))
    var res = this.directionAtoB(a.pos, b.pos)
    res.x = res.x*force
    res.y = res.y*force
    res.z = res.z*force
    res
  }

  /** Method for getting the allGravities-vector without updating it */
  def potentialGravities(a: AstralBody) = {
    var g = Vector[Vector3]()
    val others = bodies.filter(_!=a)
    for (other <- others) {
      g = g :+ gravityVectorFromAtoB(a,other)
    }
    g
  }


  /** Updates the gravities in each body's allGravities-array */
  def updateGravitiesForBodies() = {
    for (body <- bodies) {
      body.allGravities = Vector[Vector3]()       // empties the body's list of earlier gravities
      val others = bodies.filter(_!=body)    // creates a vector consisting of all the other bodies
      for (other <- others) {
        body.addGravityForce(gravityVectorFromAtoB(body, other))
      }
    }
  }

  /** Method for advancing in time for one timeStep in space. In the bodies-collection calculates and changes every body's
   *  total gravities, directions, accelerations, velocities and positions in space respectively. In the end time is increased
   *  with timeStep */
  def advanceStepInTime() = {
    this.updateGravitiesForBodies()
    for (body <- bodies) {
      body.updateDirection()
      body.updateAcceleration()
      body.updateVelocity()
      body.updatePos()

      //body.updateVelocityAndPosition()
    }
    this.time += timeStep
  }
}
