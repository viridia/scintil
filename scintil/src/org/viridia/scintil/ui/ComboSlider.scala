package org.viridia.scintil.ui

import scala.swing.{Component, TextField}
import scala.swing.event.{EditDone, MousePressed, MouseReleased, MouseDragged, MouseMoved, MouseExited}
import java.awt.{BasicStroke, Color, Dimension, Font, Graphics2D, LinearGradientPaint, Point,
    RenderingHints}
import java.awt.geom.GeneralPath
import javax.swing.{BorderFactory, SwingConstants}

object ComboSlider {
  private var textControl:TextField = new TextField {
    background = GroupableControl.fillColor
    border = BorderFactory.createLineBorder(new Color(128, 128, 128))
    peer.setHorizontalAlignment(SwingConstants.CENTER)
  }

  private def isTextControlShown(cs:ComboSlider):Boolean = textControl.peer.getParent() == cs.peer

  private def showTextControl(cs:ComboSlider) {
    if (textControl.peer.getParent() != cs) {
      hideTextControl()
      cs.peer.add(textControl.peer)
    }
    var w = math.min(100, cs.size.width - 40)
    textControl.peer.setSize(w, cs.size.height - 3)
    textControl.peer.setLocation((cs.size.width - w) / 2, 2)
    textControl.text = cs.valueAsString
    textControl.peer.setSelectionStart(0)
    textControl.peer.setSelectionEnd(textControl.text.size)
    textControl.peer.requestFocusInWindow()
  }

  private def hideTextControl() {
    val parent = textControl.peer.getParent()
    if (parent != null) {
      parent.remove(textControl.peer)
      parent.revalidate()
      parent.repaint()
    }
  }
}

abstract class ComboSlider(var caption:String) extends GroupableControl {
  private var dragged = false

  maximumSize = new Dimension(Integer.MAX_VALUE, 20)
  minimumSize = new Dimension(40, 20)
  preferredSize = new Dimension(40, 20)
  font = new Font("sans", Font.PLAIN, 12)
  focusable = true

  def isAdjusting:Boolean
  def isAdjusting_=(b:Boolean)
  def valueAsPosition:Int
  def valueAsString:String
  def valueFromString(s:String)
  def valueFromPosition(x:Int)

  override def paint(g:Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

    val baseTransform = g.getTransform()
    val w = size.width - 1;
    val h = size.height - 1;
    val r = h / 2f;
    val path = createOutline();

    g.setColor(GroupableControl.fillColor);
    g.fill(path);

    val clip = g.getClip()
    val x = valueAsPosition
    g.clipRect(0, 0, x.toInt, h)
    g.setColor(GroupableControl.thumbColor);
    g.fill(path);
    g.setClip(clip)

    g.setColor(GroupableControl.borderColor);
    g.draw(path);

    g.setColor(GroupableControl.detailColor);
    g.translate(r, r + 1)
    g.fill(GroupableControl.leftArrow);

    g.translate(w - r - r, 0)
    g.fill(GroupableControl.rightArrow);
    g.setTransform(baseTransform)

    if (!ComboSlider.isTextControlShown(this)) {
      val metrics = g.getFontMetrics(font)
      val text = caption + ": " + valueAsString
      var textWidth = metrics.stringWidth(text)
      g.setFont(font)
      g.setColor(GroupableControl.detailColor);
      g.drawString(text, (w - textWidth) / 2, (h + metrics.getAscent()) / 2)
    }
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case e: MousePressed =>  {
      dragged = ComboSlider.isTextControlShown(this)
      //dragTo(e.point)
      requestFocusInWindow()
    }
    case e: MouseDragged => {
      dragged = true
      isAdjusting = true
      dragTo(e.point)
    }
    case e: MouseReleased => {
      isAdjusting = false
      if (!dragged) {
        if (!ComboSlider.isTextControlShown(this)) {
          ComboSlider.showTextControl(this)
          listenTo(ComboSlider.textControl)
          repaint()
        }
      }
    }
    case e: MouseMoved => {}
    case e: MouseExited => {}
    case e: EditDone => {
      if (ComboSlider.isTextControlShown(this)) {
        valueFromString(ComboSlider.textControl.text)
        deafTo(ComboSlider.textControl)
        ComboSlider.hideTextControl()
      }
    }
  }

  private def dragTo(pt:Point) {
    if (enabled) {
      val w = size.width.toFloat
      valueFromPosition(pt.x);
    }
  }
}

class ComboSliderInt(caption:String="Value") extends ComboSlider(caption) with BoundedIntEditor {
  def valueAsPosition = (size.width - 1) * (value - minimum) / range
  def valueAsString = value.toString()
  def valueFromString(s:String) {
    setValue(s.trim().toInt)
  }
  def valueFromPosition(x:Int) = setValue(x * range / (size.width - 1) + minimum)
}

class ComboSliderFloat(caption:String="Value") extends ComboSlider(caption) with BoundedFloatEditor {
  var logScale = false
  def valueAsPosition = {
    val w = size.width - 1
    var v = if (logScale) math.log(value) else value
    var min = if (logScale) math.log(minimum) else minimum
    var max = if (logScale) math.log(maximum) else maximum
    (w * (v - min) / (max - min)).toInt
  }
  def valueFromPosition(x:Int) = {
    //exp(x / w * (log(max) - log(min)) + log(min)) = v
    val w:Float = size.width - 1
    if (logScale) {
      val min = math.log(minimum)
      val max = math.log(maximum)
      setValue(math.exp(x / w * (max - min) + min).toFloat)
    } else {
      setValue(x * (maximum - minimum) / w + minimum)
    }
  }
  def valueAsString = "%.2f".format(value)
  def valueFromString(s:String) {
    setValue(s.trim().toFloat)
  }
}
