package org.viridia.scintil.graph

import java.awt.RenderingHints
import java.awt.image.BufferedImage

/** Class responsible for caching the output image of each node, which can be invalidated
    when any of the node's inputs change. */
class PreviewImage {
  /** A 256 x 256 x RGB float raster that caches the output of a node. This is used as the
      input of other nodes. */
  var raster = RasterFactory.createFloatRaster(256, 256)
  /** The raster is invalidated whenever the node's inputs or parameters change. */
  var rasterValid = false

  /** A lower bit-depth version of the preview image used for drawing. */
  var previewImage:Option[BufferedImage] = None;

  /** Return the cached preview image (regenerating if necessary). */
  def getImage:BufferedImage = {
    if (previewImage.isEmpty) {
      previewImage = Some(RasterFactory.createImageFromRaster(raster))
    }
    return previewImage.get
  }

  def invalidate() {
    rasterValid = false
    previewImage = None
  }
}
