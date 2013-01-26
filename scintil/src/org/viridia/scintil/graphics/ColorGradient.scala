package org.viridia.scintil.graphics

import scala.collection.immutable.TreeMap
import org.viridia.scintil.math.MathUtils

class ColorStop(val position:Float, val color:ColorFloat) {}

class ColorGradient(stops:Array[ColorStop] = Array(
    new ColorStop(0, ColorFloat(0, 0, 0)), new ColorStop(.5f, ColorFloat(1, 0, 0)), new ColorStop(1, ColorFloat(1, 1, 1)))) {
  /** Fill in the result with the interpolated color at the given position along the gradient. */
  final def getColor(position:Float, result:ColorFloat) {
    if (position >= 1) {
      result.set(stops(stops.size - 1).color)
      return
    }
    if (position <= 0) {
      result.set(stops(0).color)
      return
    }
    var high = stops.size - 1
    var low = 0
    while (low < high) {
      val mid = (low + high + 1) / 2
      if (position >= stops(mid).position) {
        low = mid
      } else {
        high = mid - 1
      }
    }

    var lPos = stops(low).position
    var hPos = stops(low + 1).position
    require(position >= lPos)
    require(position <= hPos)

    result.set(stops(low).color)
    if (lPos + .000001 < hPos) {
      val t = MathUtils.clamp((position - lPos) / (hPos - lPos), 0, 1)
      result.lerp(stops(low + 1).color, t)
    }
  }

  final def getColor(position:Float):ColorFloat = {
    val result = ColorFloat()
    getColor(position, result);
    return result;
  }

  def toList = stops.toList

  def stopArray = stops
  def ==(other:ColorGradient):Boolean = stops.sameElements(other.stopArray)
  def !=(other:ColorGradient):Boolean = !stops.sameElements(other.stopArray)
}
