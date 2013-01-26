package org.viridia.scintil.math

object SimplexNoise {
  // Skewing and unskewing factors for 2, 3, and 4 dimensions
  private val F2:Double = 0.5 * (math.sqrt(3f) - 1.0);
  private val G2:Double = (3.0 - math.sqrt(3f)) / 6.0;
  private val grad:Array[Vector2] = Array(
    Vector2(1, 1),
    Vector2(-1, 1),
    Vector2(1, -1),
    Vector2(-1, -1),
    Vector2(1, 0),
    Vector2(-1, 0),
    Vector2(0, -1),
    Vector2(0, -1)
  )

  def noise(in:Vector2): Float = noise(in.x, in.y)

  def noise(x:Float, y:Float): Float = {
    // Skew the input space to determine which simplex cell we're in
    val s = (x + y) * F2;
    val i = MathUtils.floor((x + s).toFloat)
    val j = MathUtils.floor((y + s).toFloat)

    // Unskew the cell origin back to (x,y) space
    val t = (i + j) * G2
    val X0 = i - t
    val Y0 = j - t

    // The x,y distances from the cell origin
    val x0 = x - X0;
    val y0 = y - Y0;

    // For the 2D case, the simplex shape is an equilateral triangle.
    // Determine which simplex we are in.
    // Offsets for second (middle) corner of simplex in (i,j) coords
    // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where c = (3-sqrt(3))/6
    val (i1, j1) = if (x0 > y0) (1, 0) // lower triangle, XY order: (0,0)->(1,0)->(1,1)
    else                        (0, 1) // upper triangle, YX order: (0,0)->(0,1)->(1,1)

    // Offsets for middle corner in (x,y) unskewed coords
    val x1 = x0 - i1 + G2
    val y1 = y0 - j1 + G2

    // Offsets for last corner in (x,y) unskewed coords
    val x2 = x0 - 1. + 2. * G2
    val y2 = y0 - 1. + 2. * G2

    // Work out the hashed gradient indices of the three simplex corners
    val gi0 = permute(i, j)
    val gi1 = permute(i + i1, j + j1)
    val gi2 = permute(i + 1, j + 1)

    // Calculate the contribution from the three corners
    val n0 = {
      var t0:Double = 0.5 - x0 * x0 - y0 * y0
      if (t0 < 0) {
        0.0
      } else {
        t0 *= t0
        t0 * t0 * dot(grad(gi0 & 7), x0, y0)
      }
    }
    val n1 = {
      var t1:Double = 0.5 - x1 * x1 - y1 * y1
      if (t1 < 0)
        0.0
      else {
        t1 *= t1
        t1 * t1 * dot(grad(gi1 & 7), x1, y1)
      }
    }
    val n2 = {
      var t2:Double = 0.5 - x2 * x2 - y2 *y2
      if (t2 < 0)
        0.0
      else {
        t2 *= t2;
        t2 * t2 * dot(grad(gi2 & 7), x2, y2)
      }
    }

    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    return (70. * (n0 + n1 + n2)).toFloat;
  }

  private def dot(g:Vector2, x:Double, y:Double):Double = g.x * x + g.y * y
  private def permute(n:Int): Int = (((n * 34) + 1) * n) % 289
  private def permute(x:Int, y:Int): Int = permute(y + permute(x))
}
