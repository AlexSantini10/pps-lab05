package it.unibo.pps.ex

// Represents a vector in 2D space
// Structure: x-component and y-component
trait Vector2D:
  def x: Double
  def y: Double

  // Vector addition: (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  def +(other: Vector2D): Vector2D

  // Vector subtraction: (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  def -(other: Vector2D): Vector2D

  // Scalar multiplication: s * (x, y) = (s*x, s*y)
  def *(scalar: Double): Vector2D

  // Dot product: (x1, y1) · (x2, y2) = x1*x2 + y1*y2
  def dot(other: Vector2D): Double

  // Magnitude (length): ||(x, y)|| = sqrt(x*x + y*y)
  def magnitude: Double

object Vector2D:
  // Factory method to create Vector2D instances
  def apply(x: Double, y: Double): Vector2D = Vector2DImpl(x, y)

  // Common vectors (optional but nice)
  val zero: Vector2D = apply(0.0, 0.0)
  val i: Vector2D = apply(1.0, 0.0) // Unit vector along x-axis
  val j: Vector2D = apply(0.0, 1.0) // Unit vector along y-axis

  // Vector2DImpl
  private case class Vector2DImpl(override val x: Double, override val y: Double) extends Vector2D:
    override def +(other: Vector2D): Vector2D = Vector2DImpl(x + other.x, y + other.y)

    override def -(other: Vector2D): Vector2D = Vector2DImpl(x - other.x, y - other.y)

    override def *(scalar: Double): Vector2D = Vector2DImpl(x * scalar, y * scalar)

    override def dot(other: Vector2D): Double = (x * other.x) + (y * other.y)

    override def magnitude: Double = Math.sqrt(x * x + y * y)


/** Hints:
 *   - Implement Vector2D with a Vector2DImpl class.
 *   - Initially, use a regular `class`. Check that equality (==) and
 *      toString do not behave as you might expect for a value object representing a vector.
 *   - Modify the implementation to use a `case class Vector2DImpl` instead.
 *   - Observe how equality (==) and toString now work correctly out-of-the-box.
 */
@main def checkVectors(): Unit =
  val v1 = Vector2D(3.0, 4.0)
  val v2 = Vector2D(-1.0, 2.0)

  val sum = v1 + v2
  // Expected: (3 + (-1), 4 + 2) = (2.0, 6.0)
  println(s"Sum: $sum, x: ${sum.x}, y: ${sum.y}")

  val difference = v1 - v2
  // Expected: (3 - (-1), 4 - 2) = (4.0, 2.0)
  println(s"Difference: $difference, x: ${difference.x}, y: ${difference.y}")

  val scaled = v1 * 2.0
  // Expected: (3*2, 4*2) = (6.0, 8.0)
  println(s"Scaled: $scaled, x: ${scaled.x}, y: ${scaled.y}")

  val dotProduct = v1.dot(v2)
  // Expected: 3*(-1) + 4*2 = -3 + 8 = 5.0
  println(s"Dot Product: $dotProduct")

  val magV1 = v1.magnitude
  // Expected: sqrt(3*3 + 4*4) = sqrt(9 + 16) = sqrt(25) = 5.0
  println(s"Magnitude of v1: $magV1")

  val magV2 = v2.magnitude
  // Expected: sqrt((-1)*(-1) + 2*2) = sqrt(1 + 4) = sqrt(5) approx 2.236
  println(s"Magnitude of v2: $magV2") // Check if close to 2.236

  // Check zero vector and unit vectors if implemented in companion object
  // println(s"Zero vector: ${Vector2D.zero}")
  // println(s"Dot product v1.dot(Vector2D.i): ${v1.dot(Vector2D.i)}") // Should be v1.x = 3.0

  val multipleOps = (v1 + v2) * 3.0 - Vector2D(1.0, 1.0)
  // sum = (2.0, 6.0)
  // sum * 3.0 = (6.0, 18.0)
  // (6.0, 18.0) - (1.0, 1.0) = (5.0, 17.0)
  println(s"Multiple Ops: $multipleOps, x: ${multipleOps.x}, y: ${multipleOps.y}")
