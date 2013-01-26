package org.viridia.scintil.math

object MathUtils {
  def clamp(value:Int, min:Int, max:Int):Int =
    if (value < min) min else if (value > max) max else { value }

  def clamp(value:Short, min:Short, max:Short):Short =
    if (value < min) min else if (value > max) max else { value }

  def clamp(value:Float, min:Float, max:Float):Float =
    if (value < min) min else if (value > max) max else { value }

  def clamp(value:Double, min:Double, max:Double):Double =
    if (value < min) min else if (value > max) max else { value }

  /** Return the largest integer less than or equal to the specified float. */
  def floor(x:Float):Int = {
    return math.floor(x).toInt
  }

  /** Return the largest integer greater than or equal to the specified float. */
  def ceil(x:Float):Int = {
    return math.ceil(x).toInt
  }

  /**
   * Return 0 if v <= thresh0.
   * Return 1 if v >= thresh1.
   * If thresh0 < v < thresh1, return a value between 0 and 1 that represents where v is relative
   * to the two thresholds.
   */
  def smoothStep(v:Float, thresh0:Float, thresh1:Float):Float = {
    if (v <= thresh0) 0
    else if (v >= thresh1) 1
    else (v - thresh0) / (thresh1 - thresh0)
  }

  /** Linearly interpolate between two values. */
  def lerp(v0:Float, v1:Float, alpha:Float) = v0 * (1f - alpha) + v1 * alpha

  /** Convert RGB into a luminance value. */
  def luminance(r:Float, g:Float, b:Float) = r * .3f + g * .59f + b * .11f
}
