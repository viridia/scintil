package org.viridia.scintil.graph

import java.awt.Point
import java.awt.image.Raster
import scala.collection.immutable.ListSet

class InputTerminal(val name:String, val node:Node) {
  // List of connections leading to this terminal.
  var arc:Option[Arc] = None
  var position = new Point()

  def disconnect(arc:Arc) = {
    require(this.arc.isDefined);
    this.arc = None
    arc.dest = None
    arc.modified = true
  }

  def connect(arc:Arc) = {
    require(this.arc.isEmpty);
    arc.dest.foreach(_.disconnect(arc))
    this.arc = Some(arc)
    arc.dest = Some(this)
    arc.modified = true
  }

  def isConnected:Boolean = arc.isDefined

  def setPosition(x:Int, y:Int) = {
    if (position.x != x || position.y != y) {
      position.x = x
      position.y = y
      arc.foreach(_.modified = true)
    }
  }

  /** Iterate over all the output terminals that are connected to this input terminal. */
  def connectedOutputs = for (arc <- arc; out <- arc.source) yield out

  /** Iterate over all the outputs that are connected to this input. */
  def sources = for (arc <- arc; out <- arc.source) yield out.node
}
