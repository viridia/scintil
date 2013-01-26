package org.viridia.scintil.generators

import scala.reflect.{BeanProperty, BeanDisplayName}
import org.viridia.scintil.graph._
import org.viridia.scintil.graphics.ColorFloat
import org.viridia.scintil.math.{MathUtils, PerlinNoise, Vector3}
import java.awt.{Graphics2D, Point}
import java.awt.color._
import java.awt.image._
import java.awt.geom.AffineTransform

class PerlinNoiseGenerator extends Generator {
  def caption = "Perlin Noise"

  val scaleX = new FloatProperty("Scale X", 1f, minVal = 1f, maxVal = 1000f, logScale = true)
  val scaleY = new FloatProperty("Scale Y", 1f, minVal = 1f, maxVal = 1000f, logScale = true)
  val valueScale = new FloatProperty(
      "Value Scale", 1f, minVal = .001f, maxVal = 1000f, logScale = true)
  val peristence = new FloatProperty("Persistence", 0.5f, minVal = 0f)
  val startBand = new IntProperty("Start Band", 1, minVal = 1, maxVal = 16)
  val endBand = new IntProperty("End Band", 8, minVal = 1, maxVal = 16)
  val seed = new IntProperty("Seed", -1, minVal = -1)
  val colorGradient = new ColorGradientProperty("Color Mapping")

  // TODO: spectral control
  // TODO: periodic

  val noise = new PerlinNoise(1, -1)

  def fillRaster(rs:RasterSource, r:WritableRaster) {
    val gradient = colorGradient.value
    val color = ColorFloat()
    val pos = new Vector3
    val sx = scaleX.value
    val sy = scaleY.value
    val du = 1.0f / r.getWidth() * sx
    val dv = 1.0f / r.getHeight() * sy
    val vs = valueScale.value
    val isPeriodic = true
    var freq = math.pow(2.0f, startBand.value - 1).toFloat
    for (y <- 0 to r.getHeight() - 1) {
      val v = dv * y
      for (x <- 0 to r.getWidth() - 1) {
        val u = du * x
        var value = 0f
        var f = freq
        for (i <- startBand.value to endBand.value - 1) {
          //mult = spectralControl.get(i, 0.5f);
          val mult = math.pow(0.7f, i).toFloat
          pos.set(u * f, v * f, 0f)
          var valueAdd = if (isPeriodic)
            noise.sample3dPeriodic(pos, (f * sx).toInt, (f * sy).toInt, 256) * mult;
          else
            noise.sample(pos) * mult;
          value += valueAdd;
          f *= 2.0f;
        }
        value = MathUtils.clamp(value * 0.5f * vs + 0.5f, 0f, 1f)
        gradient.getColor(value, color)
        //value *= valueScale.get();
        r.setSample(x, y, 0, color.r)
        r.setSample(x, y, 1, color.g)
        r.setSample(x, y, 2, color.b)
      }
    }
  }

//    ColorGradientParam colorGradientParam = CreateLocalColorGradientParam("Color Mapping");
//    SpectralControlParam spectralControl = CreateLocalSpectralControlParam("Spectral Control");
//    BoolParam periodic = CreateLocalBoolParam("Periodic", true);
}
