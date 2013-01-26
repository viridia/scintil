package org.viridia.scintil.ui

import scala.swing.Component
import scala.swing.event.{MousePressed, MouseReleased, MouseDragged, MouseMoved, MouseExited}
import java.awt.{BasicStroke, Color, Dimension, Font, Graphics2D, LinearGradientPaint, Point,
    RenderingHints}
import java.awt.geom.GeneralPath
import scala.swing.Orientation

/** A slider-style control which renders a color gradient in the background. */
class GradientSlider extends Component with BoundedFloatEditor {
  private val borderColor = new Color(128, 128, 128);
  private val disabledBorderColor = new Color(192, 192, 192);

  def isHorizontal = orientation == Orientation.Horizontal
  private var gradient:LinearGradientPaint = null
  private var colors = Array[Color](Color.BLUE, Color.BLACK)
  private val thumbStrokeOuter = new BasicStroke(1);
  private val thumbStrokeInner = new BasicStroke(2);

  /** Set the gradient to be displayed in the background of this control. */
  def setGradient(colors:Array[Color]) {
    if (!this.colors.sameElements(colors)) {
      this.colors = colors;
      this.gradient = null;
      repaint();
    }
  }

  maximumSize = if (isHorizontal) new Dimension(Integer.MAX_VALUE, 18)
      else new Dimension(20, Integer.MAX_VALUE)
  minimumSize = if (isHorizontal) new Dimension(40, 18) else new Dimension(20, 40)
  preferredSize = if (isHorizontal) new Dimension(40, 18) else new Dimension(20, 40)

  /** @see javax.swing.JComponent#paint(java.awt.Graphics) */
  override def paint(g:Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

    if (colors != null && gradient == null) {
      val stops = new Array[Float](colors.length);
      for (i <- 0 to colors.length - 1) {
        stops(i) = i.toFloat / (colors.length - 1).toFloat;
      }
      if (isHorizontal) {
        gradient = new LinearGradientPaint(
            size.height / 2, 0, size.width - size.height/2, 0, stops, colors);
      } else {
        gradient = new LinearGradientPaint(
            0, size.width / 2, 0, size.height - size.width/2, stops, colors);
      }
    }

    val w = size.width - 1;
    val h = size.height - 1;
    val path = new GeneralPath();

    if (isHorizontal) {
      val h2 = h / 2f;
      val k = h2 * (1f - 0.5522f);
      path.moveTo(w - h2, 0);
      path.curveTo(w - k, 0, w, k, w, h2);
      path.curveTo(w, h - k, w - k, h, w - h2, h);
      path.lineTo(h2, h);
      path.curveTo(k, h, 0, h - k, 0, h2);
      path.curveTo(0, k, k, 0, h2, 0);
      path.lineTo(w - h2, 0);
      path.closePath();
    } else {
      val w2 = w / 2f;
      val k = w2 * (1f - 0.5522f);
      path.moveTo(0, w2);
      path.lineTo(0, h - w2);
      path.curveTo(0, h - k, k, h, w2, h);
      path.curveTo(w - k, h, w, h - k, w, h - w2);
      path.lineTo(w, w2);
      path.curveTo(w, k, w - k, 0, w2, 0);
      path.curveTo(k, 0, 0, k, 0, w2);
      path.closePath();
    }

    if (gradient != null) {
      g.setPaint(gradient);
      g.fill(path);
    }

    g.setColor(if (enabled) borderColor else disabledBorderColor);
    g.draw(path);

    if (enabled) {
      if (isHorizontal) {
        var x = ((w - h) * (value - minimum) / range + h/2).toInt;
        var y = size.height / 2 - 1;
        var r0 = size.height / 4 + 1;
        var r1 = r0 - 1;
        g.setStroke(thumbStrokeOuter);
        g.setColor(Color.BLACK);
        g.drawOval(x - r0, y - r0, r0 * 2, r0 * 2);
        g.setStroke(thumbStrokeInner);
        g.setColor(Color.WHITE);
        g.drawOval(x - r1, y - r1, r1 * 2, r1 * 2);
      } else {
        var x = size.width / 2 - 1;
        var y = (h - w/2 - (h - w) * (value - minimum) / range).toInt;
        var r0 = size.width / 4 + 1;
        var r1 = r0 - 1;
        g.setStroke(thumbStrokeOuter);
        g.setColor(Color.BLACK);
        g.drawOval(x - r0, y - r0, r0 * 2, r0 * 2);
        g.setStroke(thumbStrokeInner);
        g.setColor(Color.WHITE);
        g.drawOval(x - r1, y - r1, r1 * 2, r1 * 2);
      }
    }
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case e: MousePressed =>  {
      isAdjusting = true
      dragTo(e.point)
    }
    case e: MouseDragged => dragTo(e.point)
    case e: MouseReleased => {
      isAdjusting = false
      publish(new ValueChanged)
    }
    case e: MouseMoved => {}
    case e: MouseExited => {}
  }

  private def dragTo(pt:Point) {
    if (enabled) {
      if (isHorizontal) {
        val h = size.height.toFloat
        val h2 = h * 0.5f;
        val w = size.width.toFloat
        setValue((pt.x - h2) * range / (w - h) + minimum);
      } else {
        val w = size.width.toFloat
        val w2 = w * 0.5f;
        val h = size.height.toFloat
        setValue((h - w2 - pt.y) * range / (h - w) + minimum);
      }
    }
  }
}
