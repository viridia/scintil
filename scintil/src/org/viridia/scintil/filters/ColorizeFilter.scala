package org.viridia.scintil.filters

import org.viridia.scintil.graph.{Filter, InputTerminal, OutputTerminal, RasterSource}
import scala.reflect.BeanProperty
import java.awt.image.WritableRaster

class ColorizeFilter extends Filter {
  def caption = "Colorize"
  val input = new InputTerminal("In", this)
  override def inputs = List(input)

  def fillRaster(rs:RasterSource, r:WritableRaster) {
    val source = rs.getRaster(input)
  }
}
