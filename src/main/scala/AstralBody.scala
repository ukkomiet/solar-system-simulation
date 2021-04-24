

class AstralBody(spaceIn: Space,n: String, m: Double, r: Double, p: Vector3, vel: Vector3) {

  val space = spaceIn

  /** Key values of an astral body */
  val mass = m                               // Unit: kg
  val radius = r                             // Unit: m
  /** Position of the body in (x, y, z) */
  var pos = p                                // Unit: m ( as in distance from point (0,0,0) )

  val name = n

  /** Velocity-vector divided into three components */
  var velocity = vel                         // Unit: m

  /** contains all gravity-forces affecting this body, calculated in the Space-class */
  var allGravities = Vector[Vector3]()       // Unit: N (Newton)

  /** holds the direction the body's acceleration */
  var direction: Vector3 = new Vector3(1,0,0)

  /** holds the acceleration of the body */
  var acceleration = new Vector3(0,0,0)      // Unit: m/s^2

  /** Calculates the sum of the vectors in the allGravities-array,
   *  This resulting vector is the total gravity-force affecting the body */
  def getGravitySum: Vector3 = {             // Unit: N (Newton)
    var res = new Vector3(0,0,0)
    for (f <- allGravities) {
      res.x = res.x + f.x
      res.y = res.y + f.y
      res.z = res.z + f.z
    }
    res
  }

  /** Updates the direction-variable by normalizing the total gravity-force,
   *  resulting in the direction-vector of the body's acceleration */
  def updateDirection() = {
    direction = this.getGravitySum.normalize
  }

  /** Method for adding a force to the gravities vector */
  def addGravityForce(v: Vector3) = this.allGravities = this.allGravities :+ v

  /** Updates the acceleration of the body caused by all the forces in space */
  def updateAcceleration() = acceleration = new Vector3(direction.x*getGravitySum.magnitude/mass, direction.y*getGravitySum.magnitude/mass, direction.z*getGravitySum.magnitude/mass)

  /** Updates the velocity for a body */
  def updateVelocity() = {
    this.velocity.x += this.acceleration.x * space.timeStep
    this.velocity.y += this.acceleration.y * space.timeStep
    this.velocity.z += this.acceleration.z * space.timeStep
  }

  /** Updates the body's position in space */
  def updatePos() = {
    this.pos.x += this.velocity.x
    this.pos.y += this.velocity.y
    this.pos.z += this.velocity.z
  }



}


