package org.viridia.scintil.graph

import java.awt.Point
import scala.collection.immutable.ListSet

class OutputTerminal(val name:String, val node:Node) {
  // List of connections leading from this terminal
  var arcs = ListSet[Arc]()
  var position = new Point()

  // Disconnect an arc from this terminal
  def disconnect(arc:Arc) = {
    require(arcs contains arc);
    arcs -= arc
    arc.source = None
    arc.modified = true
  }

  // Connect an arc to this terminal
  def connect(arc:Arc) = {
    require(!(arcs contains arc));
    arc.source.foreach(_.disconnect(arc))
    arcs += arc
    arc.source = Some(this)
    arc.modified = true
  }

  def setPosition(x:Int, y:Int) = {
    if (position.x != x || position.y != y) {
      position.x = x
      position.y = y
      for (arc <- arcs) {
        arc.modified = true
      }
    }
  }

  /** Iterate over all the inputs that are connected to this output. */
  def connectedInputs = for (arc <- arcs; dest <- arc.dest) yield dest
}
