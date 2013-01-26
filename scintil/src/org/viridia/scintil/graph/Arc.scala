package org.viridia.scintil.graph
import java.awt.Point
import java.awt.Shape
import java.awt.geom.GeneralPath

class Arc(var source:Option[OutputTerminal] = None, var dest:Option[InputTerminal] = None) {
  var path:Shape = new GeneralPath
  var modified = true

  def disconnect() {
    source.foreach(_.disconnect(this))
    dest.foreach(_.disconnect(this))
  }
}
