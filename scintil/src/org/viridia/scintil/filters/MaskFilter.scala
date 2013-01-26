package org.viridia.scintil.filters

import org.viridia.scintil.graph.{Filter, InputTerminal, OutputTerminal, RasterSource}
import org.viridia.scintil.math.MathUtils._
import scala.reflect.BeanProperty
import java.awt.image.WritableRaster

class MaskFilter extends Filter {
  def caption = "Mask"
  val inputA = new InputTerminal("A", this)
  val inputB = new InputTerminal("B", this)
  val inputMix = new InputTerminal("Mix", this)
  override def inputs = List(inputA, inputB, inputMix)

  def fillRaster(rs:RasterSource, r:WritableRaster) {
    if (inputA.isConnected && inputB.isConnected && inputMix.isConnected) {
      val rasterA = rs.getRaster(inputA).get
      val rasterB = rs.getRaster(inputB).get
      val rasterMix = rs.getRaster(inputMix).get
      for (y <- 0 to r.getHeight() - 1) {
        for (x <- 0 to r.getWidth() - 1) {
          var mix = luminance(
              rasterMix.getSampleFloat(x, y, 0),
              rasterMix.getSampleFloat(x, y, 1),
              rasterMix.getSampleFloat(x, y, 2))
          val ra = rasterA.getSampleFloat(x, y, 0)
          val ga = rasterA.getSampleFloat(x, y, 1)
          val ba = rasterA.getSampleFloat(x, y, 2)
          val rb = rasterB.getSampleFloat(x, y, 0)
          val gb = rasterB.getSampleFloat(x, y, 1)
          val bb = rasterB.getSampleFloat(x, y, 2)
          r.setSample(x, y, 0, lerp(ra, rb, mix))
          r.setSample(x, y, 1, lerp(ga, gb, mix))
          r.setSample(x, y, 2, lerp(ba, bb, mix))
        }
      }
    } else if (inputA.isConnected) {
      val rasterA = rs.getRaster(inputA).get
      r.setRect(rasterA)
    } else if (inputB.isConnected) {
      val rasterB = rs.getRaster(inputB).get
      r.setRect(rasterB)
    } else if (inputMix.isConnected) {
      val rasterMix = rs.getRaster(inputB).get
      r.setRect(rasterMix)
    }
  }
}
