package org.viridia.scintil.math

/**
 * @param size
 * @param seed if seed >= 0 it is given to new Random(seed);
 *     -1 uses the values from Ken Perlin's reference implementation
 */
class PerlinNoise(size:Int, seed:Int) {

  // this is the permutation from Ken Perlin's reference implementation
  private val permutation = Array[Int](
      151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36,
      103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0,
      26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56,
      87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77,
      146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46,
      245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187,
      208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173,
      186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85,
      212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119,
      248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39,
      253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
      251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249,
      14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121,
      50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141,
      128, 195, 78, 66, 215, 61, 156, 180)

  private val p = makePermutation(seed)

  def makePermutation(seed:Int):Array[Int] = {
    if (seed < 0) {
      permutation.clone
    } else {
      val r = new scala.util.Random(seed);
      r.shuffle((0 to 255).toSeq).toArray[Int]
    }
  }

  /** @return a value between [-1,1] */
  def sample(pos:Vector3):Float = sample3d(pos, 1.0f);

  def sample3d(pos:Vector3, scale:Float):Float = {
    // Find Unit Cube containing point
    val (xi, xf) = separateFractionalPart(pos.x * size * scale)
    val (yi, yf) = separateFractionalPart(pos.y * size * scale)
    val (zi, zf) = separateFractionalPart(pos.z * size * scale)

    // compute fade curves for each of x, y, z
    val u = fade(xf); val v = fade(yf); val w = fade(zf)

    val A = perm(xi) + yi; val AA = perm(A) + zi; val AB = perm(A + 1) + zi // HASH COORDINATES OF
    val B = perm(xi + 1) + yi; val BA = perm(B) + zi; val BB = perm(B + 1) + zi; // THE 8 CUBE CORNERS,

    return lerp(w, lerp(v, lerp(u, grad(perm(AA), xf, yf, zf), // AND ADD
            grad(perm(BA), xf - 1, yf, zf)), // BLENDED
            lerp(u, grad(perm(AB), xf, yf - 1, zf), // RESULTS
                    grad(perm(BB), xf - 1, yf - 1, zf))),// FROM 8
            lerp(v, lerp(u, grad(perm(AA + 1), xf, yf, zf - 1), // CORNERS
                    grad(perm(BA + 1), xf - 1, yf, zf - 1)), // OF CUBE
                    lerp(u, grad(perm(AB + 1), xf, yf - 1, zf - 1),
                        grad(perm(BB + 1), xf - 1, yf - 1, zf - 1))));
  }

  /**
   * This method samples the perlin noise in a periodic way: the period can be given for each
   * axis separately. To work correctly: x \in [0, nx) then periodX = nx; (smaller nx are possible
   * but result in unnessecary repetion. Values larger then 256 don't change anything since the
   * noise function has an inherent period of 256.
   *
   * The periodicity is achieved by repeating the gradient vectors at the border of
   * the [0, periodX] box This is described in "Texturing and Modelling - A Procedural Approach"
   * on page 85 (top) (See also
   * http://drilian.com/category/development/graphics/procedural-textures/)
   *
   * @param pos
   * @param periodX
   * @param periodY
   * @param periodZ
   * @return
   */
  def sample3dPeriodic(pos:Vector3, periodX:Int, periodY:Int, periodZ:Int):Float = {
    // Find Unit Cube containing point
    val (xi, xf) = separateFractionalPart(pos.x * size)
    val (yi, yf) = separateFractionalPart(pos.y * size)
    val (zi, zf) = separateFractionalPart(pos.z * size)

    val Ix = xi % periodX
    val Iy = yi % periodY
    val Iz = zi % periodZ
    val Jx = (Ix + 1) % periodX
    val Jy = (Iy + 1) % periodY
    val Jz = (Iz + 1) % periodZ

    // compute fade curves for each of x, y, z
    val u = fade(xf); val v = fade(yf); val w = fade(zf);

    val A = perm(Ix); val AA = perm(A + Iy); val AB = perm((A + Jy)) // HASH COORDINATES OF
    val B = perm(Jx); val BA = perm(B + Iy); val BB = perm((B + Jy)) // THE 8 CUBE CORNERS,

    lerp(w, lerp(v, lerp(u, grad(perm(AA + Iz), xf, yf, zf), // AND ADD
                            grad(perm(BA + Iz), xf - 1, yf, zf)), // BLENDED
            lerp(u, grad(perm(AB + Iz), xf, yf - 1, zf), // RESULTS
                    grad(perm(BB + Iz), xf - 1, yf - 1, zf))),// FROM 8
            lerp(v, lerp(u, grad(perm(AA + Jz), xf, yf, zf - 1), // CORNERS
                    grad(perm(BA + Jz), xf - 1, yf, zf - 1)), // OF CUBE
                    lerp(u, grad(perm(AB + Jz), xf, yf - 1, zf - 1),
                        grad(perm(BB + Jz), xf - 1, yf - 1, zf - 1))));
  }

  private def separateFractionalPart(f:Float):(Int, Float) = {
    var xi = MathUtils.floor(f)
    return (xi & 255, f - xi)
  }

  private def perm(idx:Int):Int = p(idx & 255);
  private def fade(t:Float):Float = t * t * t * (t * (t * 6 - 15) + 10)
  private def lerp(t:Float, a:Float, b:Float):Float = a + t * (b - a)
  private def grad(hash:Int, x:Float, y:Float, z:Float):Float = {
    val h = hash & 15 // CONVERT LO 4 BITS OF HASH CODE
    val u = if (h < 8) x else y // INTO 12 GRADIENT DIRECTIONS.
    val v = if (h < 4) y else if (h == 12 || h == 14) x else z
    return (if ((h & 1) == 0) u else -u) + (if ((h & 2) == 0) v else -v);
  }
}
