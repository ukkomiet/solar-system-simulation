


class AstralBody(n: String, m: Double, r: Double, p: Vector3, velocity: Vector3) {


  /** Key values of an astral body */
  val mass = m
  val radius = r
  /** Position of the body in (x, y, z) */
  var pos = p

  val name = n

  /** Velocity-vector divided into three components */
  var velocity_x = velocity.x
  var velocty_y = velocity.y
  var velocity_z = velocity.z

  /** contains all gravity-forces affecting this body, calculated in the Space-class */
  var allGravities = Vector[Vector3]()

  /** holds the direction the body's acceleration */
  var direction: Vector3 = new Vector3(1,0,0)

  /** holds the acceleration of the body */
  var acceleration = new Vector3(0,0,0)

  /** Calculates the sum of the vectors in the allGravities-array,
   *  This resulting vector is the total gravity-force affecting the body */
  def getGravitySum: Vector3 = {
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
  def updateDirection() = direction = this.getGravitySum.normalize

  /** Method for adding a force to the gravities vector */
  def addGravityForce(v: Vector3) = this.allGravities :+ v


}


