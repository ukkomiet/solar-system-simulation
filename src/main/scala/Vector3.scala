import scala.math._


class Vector3(var x: Double, var y: Double, var z: Double) {

  /** Calculates the magnitude of the vector */
  def magnitude: Double = sqrt(pow(x,2) + pow(y,2) + pow(z,2))

  /** Normalizes the vector, resulting in a new vector with the same direction but a magnitude of 1
   *  if the magnitude of the vector is 0, then its length will always be 0 */
  def normalize: Vector3 = if (magnitude != 0) new Vector3(x/magnitude, y/magnitude, z/magnitude) else new Vector3(0,0,0)

  /** Method for adding together two vectors */
  def +(other: Vector3) = {
    val res = new Vector3(0,0,0)
    res.x = this.x + other.x
    res.y = this.y + other.y
    res.z = this.z + other.z
    res
  }
}