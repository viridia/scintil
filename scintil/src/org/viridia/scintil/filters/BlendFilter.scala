package org.viridia.scintil.filters

import org.viridia.scintil.graph.{EnumerationProperty, Filter, InputTerminal,
    OutputTerminal, RasterSource}
import org.viridia.scintil.graphics.ColorFloat
import org.viridia.scintil.math.MathUtils._
import scala.reflect.BeanProperty
import java.awt.image.WritableRaster

object BlendType extends Enumeration {
  type BlendType = Value
  // Functions (operate only on RGB); I0 = first input; I1 = second input; O = Output:\n" +
  val Identity,     // O = I0
      Multiply,     // O = I0*I1
      Screen,       // O = 1-(1-I0)*(1-I1)
      Overlay,      // O = I0*(I0 + 2*I1*(1-I0))
      Dodge,        // O = I0/((1-I1)+1)
      Burn,         // O = 1-((1-I0)/(I1+1))
      Difference,   // O = |I0-I1|
      Addition,     // O = min(1, I0+I1)
      Subtract      // O = max(0, I0-I1)
        = Value
}

class BlendFilter extends Filter {
  def caption = "Blend"
  val inputA = new InputTerminal("A", this)
  val inputB = new InputTerminal("B", this)
  override def inputs = List(inputA, inputB)
  val function = new EnumerationProperty("Function", BlendType, BlendType.Multiply)

  def fillRaster(rs:RasterSource, r:WritableRaster) {
    // Note: Don't assume these values are in the range 0..1
    val aColor = ColorFloat()
    val bColor = ColorFloat()
    val result = ColorFloat()
    val one = ColorFloat(1, 1, 1, 1)
    val zero = ColorFloat(0, 0, 0, 0)
    if (inputA.isConnected && inputB.isConnected) {
      val rasterA = rs.getRaster(inputA).get
      val rasterB = rs.getRaster(inputB).get
      for (y <- 0 to r.getHeight() - 1) {
        for (x <- 0 to r.getWidth() - 1) {
          aColor.set(
              rasterA.getSampleFloat(x, y, 0),
              rasterA.getSampleFloat(x, y, 1),
              rasterA.getSampleFloat(x, y, 2),
              1)
          bColor.set(
              rasterB.getSampleFloat(x, y, 0),
              rasterB.getSampleFloat(x, y, 1),
              rasterB.getSampleFloat(x, y, 2),
              1)
          function.value match {
            case BlendType.Identity => result.set(aColor)
            case BlendType.Multiply => result.set(aColor).mul(bColor)
            case BlendType.Screen => {
              result.r = 1f - (1f - aColor.r) * (1f - bColor.r)
              result.g = 1f - (1f - aColor.g) * (1f - bColor.g)
              result.b = 1f - (1f - aColor.b) * (1f - bColor.b)
            }
            case BlendType.Overlay => {
              result.r = aColor.r * (aColor.r + 2f * bColor.r * (1f - aColor.r))
              result.g = aColor.g * (aColor.g + 2f * bColor.g * (1f - aColor.g))
              result.b = aColor.b * (aColor.b + 2f * bColor.b * (1f - aColor.b))
            }
            case BlendType.Addition => result.set(aColor).add(bColor).min(one)
            case BlendType.Subtract => result.set(aColor).sub(bColor).max(zero)
            case _ => result.set(bColor)
          }
          r.setSample(x, y, 0, result.r)
          r.setSample(x, y, 1, result.g)
          r.setSample(x, y, 2, result.b)
        }
      }
    } else if (inputA.isConnected) {
      val rasterA = rs.getRaster(inputA).get
      r.setRect(rasterA)
    } else if (inputB.isConnected) {
      val rasterB = rs.getRaster(inputB).get
      r.setRect(rasterB)
    }
  }
}
