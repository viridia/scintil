package org.viridia.scintil.ui

import org.viridia.scintil.graph._
import org.viridia.scintil.math.MathUtils.clamp
import scala.math.abs
import scala.math.min
import scala.math.max
import scala.swing.{ Panel, ScrollPane }
import scala.swing.Color
import scala.swing.event.{ MousePressed, MouseReleased, MouseDragged, MouseMoved, MouseExited }
import java.awt.{ BasicStroke, Dimension, Font, Graphics2D, LinearGradientPaint, Point, Rectangle, RenderingHints }
import java.awt.geom.{ AffineTransform, GeneralPath }
import javax.swing.JViewport
import scala.swing.event.{ MouseExited, SelectionChanged }

class GraphView extends Panel {
  // Title Bar Geometry
  private val titleBarHeight = 20

  // Node Geometry
  private val nodeWidth = Node.previewImageSize + 9
  private val nodeHeight = Node.previewImageSize + titleBarHeight + 9
  private val nodePath = createRoundRectPath(0, 0, nodeWidth, nodeHeight, 7.0)
  private val nodeOutlinePath = createRoundRectPath(-2, -2, nodeWidth + 4, nodeHeight + 4, 9.0)
  private val nodeShadowOffset = new Point(5, 7)

  // Terminal Geometry
  private val terminalWidth = 8
  private val terminalHeight = 15
  private val inputTerminalPath = createInputTerminalPath(terminalWidth, terminalHeight, 2.0)
  private val outputTerminalPath = createOutputTerminalPath(terminalWidth, terminalHeight, 2.0)
  private val terminalTopMargin = 10
  private val terminalBottomMargin = 10
  private val terminalAreaHeight = nodeHeight - terminalTopMargin - terminalBottomMargin
  private val terminalShadowOffset = new Point(3, 5)

  // Colors
  private val shadow = new Color(70, 70, 100)
  private val fill = new Color(220, 220, 240)
  private val outline = new Color(0, 0, 0)
  private val fillShine = fill.brighter()
  private val fillShade = fill.darker()
  private val nodeFill = createNodeFillGradient
  private val inputTerminalFill = createTerminalFillGradient(terminalHeight, new Color(200, 200, 170))
  private val outputTerminalFill = createTerminalFillGradient(terminalHeight, new Color(170, 200, 200))
  private val preview = new Color(30, 30, 100)
  private val arcFill = new Color(50, 200, 50)
  private val arcDragFill = new Color(130, 240, 90)
  private val arcShine = arcFill.brighter()
  private val hiliteBorderColor = new Color(255, 255, 255)
  private val selectedBorderColor = new Color(128, 255, 128)

  // Strokes
  private val nodeOutlineStroke = new BasicStroke(2f)
  private val nodeDivideStroke = new BasicStroke(1f)
  private val terminalOutlineStroke = new BasicStroke(1f)
  private val arcStroke = new BasicStroke(7f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  private val arcInnerStroke = new BasicStroke(5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  private val arcHiliteStroke = new BasicStroke(2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  private val selectionRectStroke = new BasicStroke(1f,
    BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 0, Array[Float](5f, 3f), 0)

  // Fonts
  font = new Font("sans", Font.PLAIN, 12)
  val terminalFont = new Font("sans", Font.PLAIN, 11)

  // List of nodes
  var graph = new Graph

  abstract class Drag
  case object DragNothing extends Drag
  case class DragScroll(xOffset:Int, yOffset:Int) extends Drag
  case class DragNode(node:Node, xOffset:Int, yOffset:Int) extends Drag
  case class DragInput(node:Node, index:Int, x:Int, y:Int) extends Drag
  case class DragOutput(node:Node, x:Int, y:Int, arc:Option[Arc]=None) extends Drag
  case class DragRectSelect(anchor:Point, cursor:Point) extends Drag

  abstract class Pickable
  case object PickNothing extends Pickable
  case class PickNode(node:Node) extends Pickable
  case class PickInput(node:Node, index:Int) extends Pickable
  case class PickOutput(node:Node) extends Pickable

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  // State variables
  var drag:Drag = DragNothing
  var hiliteObject:Pickable = PickNothing
  var selectedNodes = Set[Node]()

  background = new Color(96, 96, 128)

  val scrollPane = new ScrollPane
  scrollPane.horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
  scrollPane.verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
  scrollPane.contents = this

  val origin = new Point()

  override def paint(g:Graphics2D) = {
    g.setBackground(background)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    // Update terminal positions if needed
    for (node <- graph.nodes)
      updateTerminalPositions(node)

    // Route modified paths if needed
    for (arc <- graph.arcs)
      if (arc.modified) {
        routeArcPath(arc)
        arc.modified = false
      }

    // Calculate the root transform
    val transform = g.getTransform
    transform.translate(origin.x, origin.y)

    // Render node shadows
    for (node <- graph.nodes)
      paintNodeShadows(g, node, transform)

    // Render arc shadows
    for (arc <- graph.arcs if !isDraggedArc(arc))
      paintArcShadow(g, arc, transform)

    // Render arcs
    for (arc <- graph.arcs if !isDraggedArc(arc))
      paintArc(g, arc, transform)

    paintDragHilites(g, transform)

    // Render nodes
    for (node <- graph.nodes)
      paintNode(g, node, transform)

    g.setTransform(transform)
    paintHiglights(g, transform)
  }

  // Render node shadows
  def paintNodeShadows(g:Graphics2D, node:Node, transform:AffineTransform) {
    g.setTransform(transform)
    g.translate(
        node.position.x + nodeShadowOffset.x,
        node.position.y + nodeShadowOffset.y)
    g.setPaint(shadow)
    g.fill(nodePath)

    // Render terminal shadows
    var index = 0
    node.inputs.foreach(terminal => {
      g.setTransform(transform)
      g.translate(
          node.position.x + terminalShadowOffset.x,
          node.position.y + terminalShadowOffset.y + terminalPosition(index, node.inputs.size))
      g.fill(inputTerminalPath)
      index += 1
    })

    node.output.foreach(terminal => {
      g.setTransform(transform)
      g.translate(
          node.position.x + terminalShadowOffset.x + nodeWidth,
          node.position.y + terminalShadowOffset.y + terminalPosition(0, 1))
      g.fill(outputTerminalPath)
    })
  }

  def paintArcShadow(g:Graphics2D, arc:Arc, transform:AffineTransform) {
    g.setTransform(transform)
    g.translate(2, 3)
    g.setStroke(arcStroke)
    g.draw(arc.path)
  }

  def paintArc(g:Graphics2D, arc:Arc, transform:AffineTransform) {
    g.setTransform(transform)
    g.setPaint(outline)
    g.setStroke(arcStroke)
    g.draw(arc.path)

    g.setStroke(arcInnerStroke)
    g.setPaint(arcFill)
    g.draw(arc.path)

    g.translate(-1, -1)
    g.setStroke(arcHiliteStroke)
    g.setPaint(arcShine)
    g.draw(arc.path)
  }

  def paintNode(g:Graphics2D, node:Node, transform:AffineTransform) {
    g.setTransform(transform)
    g.translate(node.position.x, node.position.y)

    // Render fill
    g.setPaint(nodeFill)
    g.fill(nodePath)

    // Render divider
    g.setStroke(nodeDivideStroke)
    g.setPaint(fillShade)
    g.fillRect(0, titleBarHeight, nodeWidth, 1)
    g.setPaint(fillShine)
    g.fillRect(0, titleBarHeight + 1, nodeWidth, 1)

    g.setFont(terminalFont)

    // Render terminals
    var index = 0
    val terminalMetrics = g.getFontMetrics(terminalFont)
    node.inputs.foreach(terminal => {
      // Position
      g.setTransform(transform)
      g.translate(node.position.x, node.position.y + terminalPosition(index, node.inputs.size))
      // Fill
      g.setPaint(inputTerminalFill)
      g.fill(inputTerminalPath)
      // Outline
      g.setStroke(terminalOutlineStroke)
      g.setPaint(outline)
      g.draw(inputTerminalPath)
      // Caption
      val strWidth = terminalMetrics.stringWidth(terminal.name)
      g.setPaint(fill)
      g.drawString(terminal.name, -strWidth - 5, -10)
      index += 1
    })

    node.output.foreach(terminal => {
      // Position
      g.setTransform(transform)
      g.translate(node.position.x + nodeWidth, node.position.y + terminalPosition(0, 1))
      // Fill
      g.setPaint(outputTerminalFill)
      g.fill(outputTerminalPath)
      // Outline
      g.setStroke(terminalOutlineStroke)
      g.setPaint(outline)
      g.draw(outputTerminalPath)
      // Caption
      g.setPaint(fill)
      g.drawString(terminal.name, 5, -10)
    })

    // Render outline
    g.setTransform(transform)
    g.translate(node.position.x, node.position.y)
    g.setStroke(nodeOutlineStroke)
    g.setPaint(outline)
    g.draw(nodePath)

    if (selectedNodes contains node) {
      g.setPaint(selectedBorderColor)
      g.draw(nodeOutlinePath)
    }

    // Render title
    g.setPaint(outline)
    g.setFont(font)

    val metrics = g.getFontMetrics(font)
    var titleWidth = metrics.stringWidth(node.caption)
    g.drawString(node.caption,
      (nodeWidth - titleWidth) / 2,
      (titleBarHeight + metrics.getHeight()) / 2)

    // Render image preview
    val preview = node.preview
    g.drawImage(preview.getImage,
      5, titleBarHeight + 4,
      Node.previewImageSize, Node.previewImageSize, null)
  }

  /** Render current highlighted object. */
  def paintHiglights(g:Graphics2D, transform:AffineTransform) {
    hiliteObject match {
      case PickNode(node:Node) => {
        g.setTransform(transform)
        g.translate(node.position.x, node.position.y)
        g.setPaint(hiliteBorderColor)
        g.draw(nodeOutlinePath)
      }

      case PickInput(node:Node, index:Int) => {
        g.setPaint(hiliteBorderColor)
        g.setTransform(transform)
        g.translate(node.position.x, node.position.y + terminalPosition(index, node.inputs.size))
        g.draw(inputTerminalPath)
      }

      case PickOutput(node:Node) => {
        g.setPaint(hiliteBorderColor)
        g.setTransform(transform)
        g.translate(node.position.x + nodeWidth, node.position.y + terminalPosition(0, 1))
        g.draw(outputTerminalPath)
      }

      case _ => Unit
    }
    g.setTransform(transform)
  }

  reactions += {
    case e:MousePressed => {
      drag = startDrag(e.point.x, e.point.y)
    }
    case e:MouseDragged => continueDrag(e.point.x, e.point.y)
    case e:MouseReleased => finishDrag(e.point.x, e.point.y)
    case e:MouseMoved => {
      val pickResult = pick(e.point.x, e.point.y)
      if (pickResult != hiliteObject) {
        repaintPickable(hiliteObject)
        hiliteObject = pickResult;
        repaintPickable(pickResult)
      }
    }
    case e:MouseExited => {
      if (hiliteObject != PickNothing) {
        repaintPickable(hiliteObject)
        hiliteObject = PickNothing
      }
    }
  }

  def startDrag(x:Int, y:Int):Drag = {
    val pickResult = pick(x, y)
    var selectionChanged = false
    return pickResult match {
      case PickNode(node) => {
        if (!(selectedNodes contains node)) {
          selectedNodes = Set(node)
          selectionChanged = true
        }
        repaint()
        if (selectionChanged) {
          publish(new SelectionChanged(this))
        }
        DragNode(node, x - node.position.x, y - node.position.y)
      }
      case PickInput(node, index) => {
        hiliteObject = PickNothing
        repaint()
        val input = node.inputs(index)
        if (input.isConnected)
            DragOutput(input.arc.get.source.get.node, x, y, input.arc)
        else DragInput(node, index, x, y)
      }
      case PickOutput(node) => {
        hiliteObject = PickNothing
        repaint()
        DragOutput(node, x, y)
      }
      case _ => {
        if (!selectedNodes.isEmpty) {
          selectedNodes = Set()
          selectionChanged = true
        }
        repaint()
        if (selectionChanged) {
          publish(new SelectionChanged(this))
        }
        DragRectSelect(new Point(x, y), new Point(x, y))
      }
    }
  }

  def continueDrag(x:Int, y:Int) {
    drag match {
      case DragNode(node, xOffset, yOffset) => {
        setNodePosition(node, x - xOffset, y - yOffset)
        repaint()
      }

      case DragInput(node, index, _, _) => {
        drag = DragInput(node, index, x, y)
        val pickResult = pick(x, y)
        pickResult match {
          case PickOutput(sourceNode) => {
            hiliteObject = if (graph.canConnect(sourceNode.output.get, node.inputs(index)))
              pickResult else PickNothing
          }
          case _ => hiliteObject = PickNothing
        }
        repaint()
      }

      case DragOutput(node, _, _, oldArc) => {
        drag = DragOutput(node, x, y, oldArc)
        val pickResult = pick(x, y)
        hiliteObject = PickNothing
        pickResult match {
          case PickInput(destNode, index) => {
            hiliteObject = if (graph.canConnect(node.output.get, destNode.inputs(index)))
              pickResult else PickNothing
          }
          case _ => hiliteObject = PickNothing
        }
        repaint()
      }

      case DragRectSelect(anchor:Point, cursor:Point) => {
        cursor.x = x
        cursor.y = y
        repaint()
      }

      case DragNothing => Unit
    }
  }

  def finishDrag(x:Int, y:Int) {
    drag match {
      case DragInput(destNode, index, x, y) => {
        hiliteObject match {
          case PickOutput(sourceNode) => {
            val input = destNode.inputs(index)
            val arc = new Arc()
            input.connect(arc)
            sourceNode.output.get.connect(arc)
            publish(new NodeChanged(destNode))
          }
          case _ => Unit
        }
        repaint()
      }

      case DragOutput(sourceNode, x, y, oldArc) => {
        oldArc.foreach(arc => { arcChanged(arc); arc.disconnect() })
        hiliteObject match {
          case PickInput(destNode, index) => {
            val input = destNode.inputs(index)
            val output = sourceNode.output.get
            if (input.isConnected) {
              // Re-use existing arc
              if (input.arc.get.source.get != output) {
                output.connect(input.arc.get)
              }
            } else {
              val arc = new Arc()
              input.connect(arc)
              output.connect(arc)
            }
            publish(new NodeChanged(destNode))
            repaint()
          }
          case _ => Unit
        }
        repaint()
      }

      case DragRectSelect(anchor:Point, cursor:Point) => {
        val x0 = min(anchor.x, cursor.x)
        val x1 = max(anchor.x, cursor.x)
        val y0 = min(anchor.y, cursor.y)
        val y1 = max(anchor.y, cursor.y)
        // Select nodes
        repaint()
      }
      case _ => {}
    }
    drag = DragNothing
    updateViewportSize()
  }

  def paintDragHilites(g:Graphics2D, transform:AffineTransform) {
    drag match {
      case DragInput(destNode, index, x, y) =>
        paintArcHighlight(g,
            createArcPath(
                hiliteObject match {
                  case PickOutput(sourceNode) => outputTerminalPosition(sourceNode)
                  case _ => new Point(x, y)
                },
                inputTerminalPosition(destNode, index)),
            transform)

      case DragOutput(sourceNode, x, y, input) => {
        paintArcHighlight(g,
            createArcPath(
                outputTerminalPosition(sourceNode),
                hiliteObject match {
                  case PickInput(destNode, index) => inputTerminalPosition(destNode, index)
                  case _ => new Point(x, y)
                }),
            transform)
      }

      case DragRectSelect(anchor:Point, cursor:Point) => {
        val x0 = min(anchor.x, cursor.x)
        val x1 = max(anchor.x, cursor.x)
        val y0 = min(anchor.y, cursor.y)
        val y1 = max(anchor.y, cursor.y)
        g.setPaint(background.brighter())
        g.setStroke(selectionRectStroke)
        g.drawRect(x0, y0, x1 - x0, y1 - y0)
      }
      case _ => Unit
    }
  }

  def paintArcHighlight(g:Graphics2D, path:GeneralPath, transform:AffineTransform) {
    g.setTransform(transform)
    g.setPaint(outline)
    g.setStroke(arcStroke)
    g.draw(path)

    g.setStroke(arcInnerStroke)
    g.setPaint(arcDragFill)
    g.draw(path)

    g.translate(-1, -1)
    g.setStroke(arcHiliteStroke)
    g.setPaint(arcDragFill.brighter())
    g.draw(path)
  }

  def pick(x:Int, y:Int):Pickable = {
    // We need to find the *last* node (since nodes are drawn from head to tail.)
    var result:Pickable = PickNothing
    for (node <- graph.nodes) {
      val pickResult = pick(x, y, node)
      if (pickResult != PickNothing) result = pickResult
    }
    return result
  }

  def pick(x:Int, y:Int, node:Node):Pickable = {
    if (y >= node.position.y && y <= node.position.y + nodeHeight) {
      // Input terminals
      if (x >= node.position.x - 20 && x <= node.position.x + 10) {
        var index = 0
        node.inputs.foreach(terminal => {
          val terminalPos = node.position.y + terminalPosition(index, node.inputs.size)
          if (y >= terminalPos - 8 && y <= terminalPos + 8) {
            return PickInput(node, index)
          }
          index += 1
        })
      }
      // Output terminal
      if (x >= node.position.x + nodeWidth - 10 && x <= node.position.x + nodeWidth + 20) {
        if (node.output.isDefined) {
          val terminalPos = node.position.y + terminalPosition(0, 1)
          if (y >= terminalPos - 8 && y <= terminalPos + 8) {
            return PickOutput(node)
          }
        }
      }
      // Node body
      if (x >= node.position.x && x <= node.position.x + nodeWidth &&
        y >= node.position.y && y <= node.position.y + nodeHeight) {
        return PickNode(node)
      }
    }
    return PickNothing
  }

  def repaintPickable(p:Pickable) {
    p match {
      case PickNode(node:Node) =>
        repaint(new Rectangle(node.position.x - 3, node.position.y - 3,
            nodeWidth + 7, nodeHeight + 7))

      case PickInput(node:Node, index:Int) => {
        val yPos = terminalPosition(index, node.inputs.size)
        repaint(new Rectangle(node.position.x - 10, node.position.y + yPos - 10, 11, 20))
      }

      case PickOutput(node:Node) => {
        repaint(new Rectangle(
            node.position.x + nodeWidth - 1, node.position.y + terminalPosition(0, 1) - 10, 11, 20))
      }

      case _ => Unit
    }
  }

  def addNode(node:Node) {
    node.position.x = (size.width - nodeWidth) / 2
    node.position.y = (size.height - nodeHeight) / 2
    graph.addNode(node)
    selectedNodes = Set(node)
    publish(new SelectionChanged(this))
    repaint()
  }

  def terminalPosition(n:Int, count:Int):Int = {
    var spacing = terminalAreaHeight / count
    return terminalTopMargin + spacing * n + spacing / 2
  }

  def inputTerminalPosition(node:Node, index:Int):Point = new Point(
      node.position.x, node.position.y + terminalPosition(index, node.inputs.size))

  def outputTerminalPosition(node:Node):Point = new Point(
      node.position.x + nodeWidth, node.position.y + terminalPosition(0, 1))

  def setNodePosition(node:Node, x:Int, y:Int) {
    node.position.x = x
    node.position.y = y
    updateTerminalPositions(node)
  }

  def updateTerminalPositions(node:Node) {
    var index = 0
    node.inputs.foreach(terminal => {
      terminal.setPosition(
        node.position.x,
        node.position.y + terminalPosition(index, node.inputs.size))
      index += 1
    })
    node.output.foreach(terminal => {
      terminal.setPosition(
        node.position.x + nodeWidth,
        node.position.y + terminalPosition(0, 1))
    })
  }

  def updateViewportSize() {
    val ul = new Point(0, 0)
    val lr = new Point(0, 0)
    for (node <- graph.nodes) {
      ul.x = min(ul.x, node.position.x - 40)
      ul.y = min(ul.y, node.position.y - 40)
      lr.x = max(lr.x, node.position.x + nodeWidth + 40)
      lr.y = max(lr.y, node.position.y + nodeHeight + 40)
    }
    // If ul is negative -- move all the graph nodes?
    preferredSize = new Dimension(lr.x - ul.x, lr.y - ul.y)
    revalidate()
  }

  def createRoundRectPath(x:Int, y:Int, w:Int, h:Int, radius:Double):GeneralPath = {
    val x0 = x
    val x1 = x0 + w
    val y0 = y
    val y1 = y0 + h

    val p = new GeneralPath()
    p.moveTo(x1 - radius, y0)
    p.curveTo(x1 - radius / 2, y0, x1, y0 + radius / 2, x1, y0 + radius)
    p.lineTo(x1, y1 - radius)
    p.curveTo(x1, y1 - radius / 2, x1 - radius / 2, y1, x1 - radius, y1)
    p.lineTo(x0 + radius, y1)
    p.curveTo(x0 + radius / 2, y1, x0, y1 - radius / 2, x0, y1 - radius)
    p.lineTo(x0, y0 + radius)
    p.curveTo(x0, y0 + radius / 2, x0 + radius / 2, y0, x0 + radius, y0)
    p.closePath()
    p
  }

  def createOutputTerminalPath(w:Int, h:Int, radius:Double):GeneralPath = {
    val x0 = 0
    val x1 = w
    val y0 = -h / 2
    val y1 = h / 2

    val p = new GeneralPath()
    p.moveTo(x1 - radius, y0)
    p.curveTo(x1 - radius / 2, y0, x1, y0 + radius / 2, x1, y0 + radius)
    p.lineTo(x1, y1 - radius)
    p.curveTo(x1, y1 - radius / 2, x1 - radius / 2, y1, x1 - radius, y1)
    p.lineTo(x0, y1)
    p.lineTo(x0, y0)
    p.closePath()
    p
  }

  def createInputTerminalPath(w:Int, h:Int, radius:Double):GeneralPath = {
    val x0 = -w
    val x1 = 0
    val y0 = -h / 2
    val y1 = h / 2

    val p = new GeneralPath()
    p.moveTo(x1, y0)
    p.lineTo(x1, y1)
    p.lineTo(x0 + radius, y1)
    p.curveTo(x0 + radius / 2, y1, x0, y1 - radius / 2, x0, y1 - radius)
    p.lineTo(x0, y0 + radius)
    p.curveTo(x0, y0 + radius / 2, x0 + radius / 2, y0, x0 + radius, y0)
    p.closePath()
    p
  }

  def routeArcPath(arc:Arc) {
    val source = arc.source.get
    val dest = arc.dest.get
    arc.path = createArcPath(source.position, dest.position)
  }

  def createArcPath(source:Point, dest:Point):GeneralPath = {
    val p = new GeneralPath
    val dx = dest.x - source.x
    val dy = dest.y - source.y
    val adx = abs(dx)
    val ady = abs(dy)
    p.moveTo(source.x, source.y)
    val l1 = clamp(max(adx * .4f, ady * .2f + 10), 10, 16)
    val dl = max(dx - l1 - l1, ady - 60)
    val l2 = clamp(dl * .5f, l1, 60)
    p.lineTo(source.x + l1, source.y)
    p.curveTo(source.x + l1 + l2, source.y, dest.x - l1 - l2, dest.y, dest.x - l1, dest.y)
    p.lineTo(dest.x, dest.y)
    return p
  }

  def arcChanged(arc:Arc) {
    arc.modified = true
    arc.dest.foreach(output => publish(new NodeChanged(output.node)))
  }

  def isDraggedArc(arc:Arc):Boolean = {
    drag match {
      case DragOutput(node, x, y, oldArc) => oldArc.exists(_ == arc)
      case _ => false
    }
  }

  private def createNodeFillGradient:LinearGradientPaint = {
    val colors = Array(fillShine, fill, fill, fillShade)
    var stops = Array(0f, .07f, .93f, 1f)
    new LinearGradientPaint(0, 0, 0, nodeHeight, stops, colors)
  }

  private def createTerminalFillGradient(height:Int, color:Color):LinearGradientPaint = {
    val colors = Array(color.brighter(), color, color.darker())
    var stops = Array(0f, .5f, 1f)
    new LinearGradientPaint(0, -height / 2, 0, height / 2, stops, colors)
  }
}
