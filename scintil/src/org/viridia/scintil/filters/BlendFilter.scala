package org.viridia.scintil.filters

import org.viridia.scintil.graph.{Filter, InputTerminal,
    OutputTerminal, RasterSource}
import org.viridia.scintil.math.MathUtils._
import scala.reflect.BeanProperty
import java.awt.image.WritableRaster

object BlendType extends Enumeration {
  type BlendType = Value
  // Functions (operate only on RGB); I0 = first input; I1 = second input; O = Output:\n" +
  val Identity,     // O = I0
      Multiply,     // O = I0*I1
      Divide,       // O = I1/(I0+1)
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

//  val blendMode = new EnumerationProperty("Blend Mode", value = BlendType.Identity)

  def fillRaster(rs:RasterSource, r:WritableRaster) {
    if (inputA.isConnected && inputB.isConnected) {
      val rasterA = rs.getRaster(inputA).get
      val rasterB = rs.getRaster(inputB).get
      for (y <- 0 to r.getHeight() - 1) {
        for (x <- 0 to r.getWidth() - 1) {
          val ra = rasterA.getSampleFloat(x, y, 0)
          val ga = rasterA.getSampleFloat(x, y, 1)
          val ba = rasterA.getSampleFloat(x, y, 2)
          val rb = rasterB.getSampleFloat(x, y, 0)
          val gb = rasterB.getSampleFloat(x, y, 1)
          val bb = rasterB.getSampleFloat(x, y, 2)
//          r.setSample(x, y, 0, lerp(ra, rb, mix))
//          r.setSample(x, y, 1, lerp(ga, gb, mix))
//          r.setSample(x, y, 2, lerp(ba, bb, mix))
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
