package org.viridia.scintil.ui

import scala.swing.event.Event
import org.viridia.scintil.graph.Node

case class NodeChanged(val node:Node) extends Event
