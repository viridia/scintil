package org.viridia.scintil.ui

import scala.collection.mutable.ArrayBuffer
import scala.swing.GridBagPanel
import scala.swing.{ Action, Insets, Component, Panel }
import scala.swing.event.{ MousePressed, MouseReleased, MouseDragged, MouseMoved, MouseExited }
import java.awt.{ BasicStroke, Color, Dimension, Graphics2D, RenderingHints }
import javax.swing.{ BorderFactory, ImageIcon }
import java.awt.LinearGradientPaint
import java.awt.geom.AffineTransform
import org.viridia.scintil.graphics.{ ColorStop, ColorFloat, ColorGradient }
import org.viridia.scintil.math.MathUtils
import org.viridia.scintil.graphics.ColorGradient

class ColorGradientEditor(caption:String) extends GridBagPanel with HSLSliders {
  object Label extends GroupableLabel(caption) {
    groupBegin = true
    minimumSize = new Dimension(20, 20)
    preferredSize = new Dimension(120, 20)
  }
  object ColorStopsControl extends GroupableControl {
    minimumSize = new Dimension(20, 30)
    preferredSize = new Dimension(120, 30)
    groupBegin = false
    private val borderStroke = new BasicStroke(1);
    private val stopIcon = getIcon("colorstop.png")
    private val stopIconHilite = getIcon("colorstop-hilite.png")
    private val stopIconSelected = getIcon("colorstop-select.png")
    private[ColorGradientEditor] var stops:ArrayBuffer[ColorStop] = ArrayBuffer(
        new ColorStop(0, ColorFloat(0, 0, 0)),
        new ColorStop(1, ColorFloat(1, 1, 1)))
    private var selectedStop = -1
    private var hilightStop = -1
    private var pickPos:Float = 0
    private var dragOffset = 0f
    private var dragLowerBound = 0f
    private var dragUpperBound = 0f
    private object AddStopMenu extends PopupMenu

    AddStopMenu += new Action("Add Stop") {
      def apply() = insertStop(pickPos)
    }
    private object RemoveStopMenu extends PopupMenu
    RemoveStopMenu += new Action("Remove Stop") {
      def apply() = removeStop(selectedStop)
    }

    override def paint(g:Graphics2D) {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      val baseTransform = g.getTransform
      val w = size.width - 1;
      val h = size.height - 1;
      val r = h / 2f;
      val path = createOutline();

      g.setPaint(createGradient());
      g.fill(path);

      var index = 0
      for (stop <- stops) {
        paintStop(g, baseTransform, stop.position, stop.color.toColor(), index)
        index += 1
      }

      g.setStroke(borderStroke)
      g.setTransform(baseTransform)
      g.setColor(GroupableControl.borderColor);
      g.draw(path);
    }

    private def paintStop(
        g:Graphics2D, transform:AffineTransform, pos:Float, color:Color, index:Int) {
      val x = ((size.width - 2) * pos + 2).toInt
      val y = size.height
      val image = if (index == hilightStop) stopIconHilite else stopIcon
      g.drawImage(image, x - 7, y - 18, this.peer)
      g.setColor(color)
      g.fillRect(x - 4, y - 10, 6, 7)
    }

    private def getIcon(path:String) =
        new ImageIcon(getClass.getResource("/rsc/icons/" + path)).getImage()

    def createGradient():LinearGradientPaint = {
      new LinearGradientPaint(
        0, 0, size.width - 1, 0,
        stops.map(_.position).toArray,
        stops.map(_.color.toColor()).toArray);
    }

    def setSelectedStop(index:Int) {
      if (selectedStop != index) {
        selectedStop = index
        if (selectedStop >= 0) {
          if (selectedStop == 0) {
            dragLowerBound = 0f
            dragUpperBound = 0f
          } else if (selectedStop == stops.size - 1) {
            dragLowerBound = 1f
            dragUpperBound = 1f
          } else {
            dragLowerBound = stops(selectedStop - 1).position + .00001f
            dragUpperBound = stops(selectedStop + 1).position - .00001f
          }
          setSelectedColor(stops(selectedStop).color)
          ColorStopPosition.minimum = dragLowerBound
          ColorStopPosition.maximum = dragUpperBound
          ColorStopPosition.value = stops(selectedStop).position
          ColorStopPosition.enabled = true
          hueSlider.enabled = true
          satSlider.enabled = true
          brtSlider.enabled = true
          isAdjusting = true
        } else {
          ColorStopPosition.minimum = 0
          ColorStopPosition.maximum = 0
          ColorStopPosition.value = 0
          ColorStopPosition.enabled = false
          hueSlider.enabled = false
          satSlider.enabled = false
          brtSlider.enabled = false
        }
        // TODO: Disable sliders if no stop selected?

        repaint()
      }
    }

    def setStopColor(color:ColorFloat) {
      if (selectedStop >= 0) {
        val prevStop = stops(selectedStop)
        stops = stops.updated(selectedStop, new ColorStop(prevStop.position, color))
        repaint()
      }
    }

    listenTo(mouse.clicks)
    listenTo(mouse.moves)

    reactions += {
      case e:MousePressed => {
        pickPos = MathUtils.clamp((e.point.x + 1f) / (size.width - 2f), 0, 1)
        setSelectedStop(hilightStop)
        if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
          if (selectedStop < 0) {
            AddStopMenu.show(this, e.point.x, e.point.y)
          } else if (selectedStop > 0 && selectedStop < stops.size - 1) {
            RemoveStopMenu.show(this, e.point.x, e.point.y)
          }
        } else if (e.clicks == 2) {
          insertStop(pickPos)
        }
      }
      case e:MouseDragged => {
        if (selectedStop > 0 && selectedStop < stops.size - 1) {
          val pos = MathUtils.clamp((e.point.x + 1f) / (size.width - 2f), dragLowerBound, dragUpperBound)
          stops = stops.updated(selectedStop, new ColorStop(pos, stops(selectedStop).color))
          ColorStopPosition.value = pos
          repaint()
        }
      }
      case e:MouseReleased => {
        isAdjusting = false
        ColorGradientEditor.this.publish(new ValueChanged)
      }
      case e:MouseMoved => {
        pickPos = MathUtils.clamp((e.point.x + 1f) / (size.width - 2f), 0, 1)
        var bestIndex = -1
        var bestDist = 10f / size.width // Maximum pick distance
        var index = 0
        for (stop <- stops) {
          val dist = math.abs(pickPos - stop.position)
          if (dist < bestDist) {
            bestDist = dist
            bestIndex = index
            dragOffset = pickPos - stop.position
          }
          index += 1
        }

        if (hilightStop != bestIndex) {
          hilightStop = bestIndex
          repaint()
        }
      }
      case e:MouseExited => {
        if (hilightStop != -1) {
          hilightStop = -1
          repaint()
        }
      }
    }

    def insertStop(pos:Float) {
      val position = MathUtils.clamp(pos, .00001f, .99999f)
      val color = toGradient.getColor(pos)
      val index = stops.indexWhere(_.position >= position)
      require(index > 0)
      require(index < stops.size)
      stops.insert(index, new ColorStop(position, color))
      setSelectedStop(index)
      ColorGradientEditor.this.publish(new ValueChanged)
    }

    def removeStop(index:Int) {
      require(index > 0)
      require(index < stops.size - 1)
      stops.remove(index)
      repaint()
      ColorGradientEditor.this.publish(new ValueChanged)
    }
  }
  object ColorStopPosition extends ComboSliderFloat {
    caption = "Position"
    groupEnd = true
  }
  val preview = new GroupableControl() {
    groupBegin = true
    groupEnd = true
    private val outerStroke = new BasicStroke(2);
    private val innerStroke = new BasicStroke(2);
    override def paint(g:Graphics2D) {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      val baseTransform = g.getTransform
      val w = size.width;
      val h = size.height;
      val r = 10;
      val outerPath = GroupableControl.createOutline(0, 0, w, h, 8, true, true);
      val innerPath = GroupableControl.createOutline(2, 2, w - 4, h - 4, 5, true, true);
      var gradient = new LinearGradientPaint(
          w - h/5, -w/5, w + h/5, w/5,
          Array(0, 1),
          Array(new Color(200, 200, 200), new Color(255, 255, 255)))

      g.setStroke(outerStroke)
      g.setPaint(gradient)
      g.draw(outerPath)

      g.setColor(background)
      g.fill(innerPath)

      g.setStroke(innerStroke)
      g.setColor(Color.BLACK)
      g.draw(innerPath)
    }
  }

  var isAdjusting = false

  layout(Label) = new Constraints {
    gridx = 0; gridy = 0; gridwidth = 2
    insets = new Insets(0, 0, 0, 0)
    fill = GridBagPanel.Fill.Both
  }
  layout(ColorStopsControl) = new Constraints {
    gridx = 0; gridy = 1; gridwidth = 2
    insets = new Insets(0, 0, 0, 0)
    fill = GridBagPanel.Fill.Both
  }
  layout(ColorStopPosition) = new Constraints {
    gridx = 0; gridy = 2; gridwidth = 2
    insets = new Insets(0, 0, 0, 0)
    fill = GridBagPanel.Fill.Both
  }
  layout(hueSlider) = new Constraints {
    gridx = 0; gridy = 3; weightx = 4
    insets = new Insets(4, 0, 0, 0)
    fill = GridBagPanel.Fill.Both
  }
  layout(satSlider) = new Constraints {
    gridx = 0; gridy = 4; weightx = 4
    insets = new Insets(3, 0, 0, 0)
    fill = GridBagPanel.Fill.Both
  }
  layout(brtSlider) = new Constraints {
    gridx = 0; gridy = 5; weightx = 4
    insets = new Insets(3, 0, 0, 0)
    fill = GridBagPanel.Fill.Both
  }
  layout(preview) = new Constraints {
    gridx = 1; gridy = 3; gridheight = 3; weightx = 1;
    insets = new Insets(3, 3, 0, 0); fill = GridBagPanel.Fill.Both
  }

  override def displayColor() {
    super.displayColor()
    preview.background =
        new Color(Color.HSBtoRGB(hueSlider.value, satSlider.value, brtSlider.value))
  }

  override def colorChanged(isAdjusting:Boolean) {
    displayColor()
    ColorStopsControl.setStopColor(ColorFloat(value))
    if (!isAdjusting) {
      publish(new ValueChanged)
    }
  }

  def setSelectedColor(color:ColorFloat) {
    value = color.toColor()
    displayColor()
  }

  def setStops(stops:Seq[ColorStop]) {
    ColorStopsControl.stops.clear()
    ColorStopsControl.stops.appendAll(stops)
  }

  def setStops(value:ColorGradient) {
    ColorStopsControl.stops.clear()
    ColorStopsControl.stops.appendAll(value.toList)
    ColorStopsControl.repaint()
//    displayColor()
  }

  def toGradient:ColorGradient = new ColorGradient(ColorStopsControl.stops.toArray)

  listenTo(ColorStopPosition)

  displayColor()
  ColorStopsControl.setSelectedStop(0)
}
