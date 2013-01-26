package org.viridia.scintil.ui

import java.awt.geom.GeneralPath

/** Fluent interface for building GeneralPaths. */
object PathBuilder {
  def apply() = new PathBuilder()
}

class PathBuilder {
  private val path = new GeneralPath()
  def close():PathBuilder = { path.closePath(); this }
  def moveTo(x:Double, y:Double):PathBuilder = { path.moveTo(x, y); this }
  def lineTo(x:Double, y:Double):PathBuilder = { path.lineTo(x, y); this }
  def quadTo(x:Double, y:Double, x2:Double, y2:Double):PathBuilder = {
    path.quadTo(x, y, x2, y2)
    this
  }
  def curveTo(x:Double, y:Double, x2:Double, y2:Double, x3:Double, y3:Double):PathBuilder = {
    path.curveTo(x, y, x2, y2, x3, y3)
    this
  }
  def getPath:GeneralPath = path
}
