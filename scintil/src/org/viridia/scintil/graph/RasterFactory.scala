package org.viridia.scintil.graph

import java.awt.{Color, Point};
import java.awt.image._

/** Object that handles allocating new rasters with the correct color model and format. */
object RasterFactory {
  val origin = new Point()
  val colorSpace = Color.WHITE.getColorSpace()
  val floatColorModel = new ComponentColorModel(
      colorSpace, false, true,
      java.awt.Transparency.OPAQUE, DataBuffer.TYPE_FLOAT)

  val byteColorModel = new ComponentColorModel(
      colorSpace, false, true,
      java.awt.Transparency.OPAQUE, DataBuffer.TYPE_BYTE)

  def createFloatRaster(w:Int, h:Int):WritableRaster = {
    val sm = new BandedSampleModel(DataBuffer.TYPE_FLOAT, 256, 256, 3)
    val buffer = sm.createDataBuffer()
    return Raster.createWritableRaster(sm, buffer, origin)
  }

  def createByteRaster(w:Int, h:Int):WritableRaster = {
    val sm = new BandedSampleModel(DataBuffer.TYPE_FLOAT, 256, 256, 3)
    val buffer = sm.createDataBuffer()
    return Raster.createWritableRaster(sm, buffer, origin)
  }

  def createImageFromRaster(r:Raster):BufferedImage = {
    val sm = new BandedSampleModel(DataBuffer.TYPE_BYTE, r.getWidth(), r.getHeight(), 3)
    val buffer = sm.createDataBuffer()
    val raster = Raster.createWritableRaster(sm, buffer, origin)

    val cconv = new ColorConvertOp(colorSpace, colorSpace, null)
    val convImage = cconv.filter(r, raster)
    return new BufferedImage(byteColorModel, raster, false, null)
  }
}
