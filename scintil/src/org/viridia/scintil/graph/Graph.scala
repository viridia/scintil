package org.viridia.scintil.graph

import org.viridia.scintil.filters.{ BlendFilter }
import org.viridia.scintil.generators.{ PerlinNoiseGenerator }

/** A graph consists of nodes and arcs. */
class Graph {
  var nodes = List[Node]()

  nodes ::= new PerlinNoiseGenerator()
  nodes.head.position.x = 100
  nodes.head.position.y = 100

  nodes ::= new BlendFilter()
  nodes.head.position.x = 300
  nodes.head.position.y = 200

  val arc = new Arc
  nodes.head.inputs(0).connect(arc)
  nodes.tail.head.output.get.connect(arc)

  def arcs = for (node <- nodes; input <- node.inputs; arc <- input.arc) yield arc

  def addNode(node: Node) = {
    nodes ::= node
  }

  def removeNode(node: Node) = {
  }

  def canConnect(output:OutputTerminal, input:InputTerminal):Boolean = {
    var visited = Set[InputTerminal]()
    var cycleDetected = false
    def cycleCheck(in:InputTerminal) {
      if (!(visited contains in)) {
        visited += in
        if (in.node.output.exists(_ == output)) {
          cycleDetected = true
          return
        }
        for (n <- in.sources; in <- n.inputs) {
          cycleCheck(in)
        }
      }
    }
    cycleCheck(input)
    return !cycleDetected
  }
}
