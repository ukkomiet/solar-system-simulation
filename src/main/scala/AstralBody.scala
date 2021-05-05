
import scala.math._

class AstralBody(spaceIn: Space,n: String, m: Double, r: Double, drawR: Long, p: Vector3, vel: Vector3) {

  val space = spaceIn

  /** Key values of an astral body */
  val mass = m                               // Unit: kg
  val radius = r                             // Unit: m
  /** Position of the body in (x, y, z) */
  var pos = p                                // Unit: m ( as in distance from point (0,0,0) )
  /** Defines how big the drawn body is */
  var drawRadius = drawR

  val name = n

  var isSatellite: Boolean = false
  var isNotSatellite = !isSatellite

  /** Velocity-vector divided into three components */
  var velocity = vel                         // Unit: m

  /** contains all gravity-forces affecting this body, calculated in the Space-class */
  var allGravities = Vector[Vector3]()       // Unit: N (Newton)

  /** holds the direction of the body's acceleration */
  var direction: Vector3 = new Vector3(1,0,0)

  /** holds the direction of the body's velocity */
  var velocityDirection: Vector3 = new Vector3(0,0,0)

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

  /** Updates the velocity for a body using Euler's simple method */
  def updateVelocity() = {
    this.velocity.x += this.acceleration.x * space.timeStep
    this.velocity.y += this.acceleration.y * space.timeStep
    this.velocity.z += this.acceleration.z * space.timeStep
    this.velocityDirection = velocity.normalize
  }

  /** Updates the body's position in space using Euler's simple method */
  def updatePos() = {
    this.pos.x += this.velocity.x * space.timeStep
    this.pos.y += this.velocity.y * space.timeStep
    this.pos.z += this.velocity.z * space.timeStep
  }


  /** Method implementing the Runge-Kutta method for calculating the updating
   *  velocities and positions for each dimension axis and updating them */
  def updateVelocityAndPosition() = {

    this.updateAcceleration()
    val h = this.space.timeStep
    val init_pos = this.pos

    // initial k-coefficients
    val k1v_x = acceleration.x
    val k1v_y = acceleration.y
    val k1v_z = acceleration.z

    val k1r_x = velocity.x
    val k1r_y = velocity.y
    val k1r_z = velocity.z

    // temp position change 1:
    this.pos.x = init_pos.x + k1r_x*h/2
    this.pos.y = init_pos.y + k1r_y*h/2
    this.pos.z = init_pos.z + k1r_z*h/2
    space.updateGravitiesForBodies()
    this.updateDirection()
    this.updateAcceleration()

    // second k-coefficients
    val k2v_x = acceleration.x
    val k2v_y = acceleration.y
    val k2v_z = acceleration.z

    val k2r_x = velocity.x + k1v_x*h/2
    val k2r_y = velocity.y + k1v_y*h/2
    val k2r_z = velocity.z + k1v_z*h/2

    // temp position change 2:
    this.pos.x = init_pos.x + k2r_x*h/2
    this.pos.y = init_pos.y + k2r_y*h/2
    this.pos.z = init_pos.z + k2r_z*h/2
    space.updateGravitiesForBodies()
    this.updateDirection()
    this.updateAcceleration()

    // third k-coefficients
    val k3v_x = acceleration.x
    val k3v_y = acceleration.y
    val k3v_z = acceleration.z

    val k3r_x = velocity.x + k2v_x*h/2
    val k3r_y = velocity.y + k2v_y*h/2
    val k3r_z = velocity.z + k2v_z*h/2

    // temp position change 3:
    this.pos.x = init_pos.x + k3r_x*h
    this.pos.y = init_pos.y + k3r_y*h
    this.pos.z = init_pos.z + k3r_z*h
    space.updateGravitiesForBodies()
    this.updateDirection()
    this.updateAcceleration()

    // fourth k-coefficients
    val k4v_x = acceleration.x
    val k4v_y = acceleration.y
    val k4v_z = acceleration.z

    val k4r_x = velocity.x + k3v_x*h
    val k4r_y = velocity.y + k3v_y*h
    val k4r_z = velocity.z + k3v_z*h

    // final updates
    this.pos = init_pos
    space.updateGravitiesForBodies()
    this.updateDirection()
    this.updateAcceleration()

    this.velocity.x = this.velocity.x + (h/6) * (k1v_x + 2*k2v_x + 2*k3v_x + k4v_x)
    this.velocity.y = this.velocity.y + (h/6) * (k1v_y + 2*k2v_y + 2*k3v_y + k4v_y)
    this.velocity.z = this.velocity.z + (h/6) * (k1v_z + 2*k2v_z + 2*k3v_z + k4v_z)

    this.pos.x = this.pos.x + (h/6) * (k1r_x + 2*k2r_x + 2*k3r_x + k4r_x)
    this.pos.y = this.pos.y + (h/6) * (k1r_y + 2*k2r_y + 2*k3r_y + k4r_y)
    this.pos.z = this.pos.z + (h/6) * (k1r_z + 2*k2r_z + 2*k3r_z + k4r_z)


  }
}

