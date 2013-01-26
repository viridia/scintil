package org.viridia.scintil.ui

import scala.swing.Component
import java.awt.{ Color, Dimension, Font, Graphics2D, RenderingHints }
import java.awt.geom.GeneralPath

object GroupableControl {
  val fillColor = new Color(220, 220, 220);
  val borderColor = new Color(150, 150, 150);
  val detailColor = new Color(64, 64, 64);
  val thumbColor = new Color(235, 235, 240);
  var leftArrow:GeneralPath = {
    val path = new GeneralPath()
    path.moveTo(6,-4)
    path.lineTo(0, 0)
    path.lineTo(6, 4)
    path.closePath()
    path
  }
  var rightArrow:GeneralPath = {
    val path = new GeneralPath()
    path.moveTo(-6, 4)
    path.lineTo(-0, 0)
    path.lineTo(-6,-4)
    path.closePath()
    path
  }

  def createOutline(
      x:Int, y:Int, w:Int, h:Int, r:Int, roundTop:Boolean, roundBottom:Boolean):GeneralPath = {
    val k = r * (1f - 0.5522f);
    val path = new GeneralPath();
    if (roundTop) {
      path.moveTo(x + w - r, y)
      path.curveTo(x + w - k, y, x + w, y + k, x + w, y + r)
      path.lineTo(x + w, y + h - r)
    } else {
      path.moveTo(x + w, y)
      path.lineTo(x + w, y + r)
    }
    if (roundBottom) {
      path.curveTo(x + w, y + h - k, x + w - k, y + h, x + w - r, y + h)
      path.lineTo(x + r, y + h)
      path.curveTo(x + k, y + h, x, y + h - k, x, y + h - r)
      path.lineTo(x, y + r)
    } else {
      path.lineTo(x + w, y + h + 1)
      path.lineTo(x, y + h + 1)
      path.lineTo(x, y + r)
    }
    if (roundTop) {
      path.curveTo(x, y + k, x + k, y, x + r, y)
    } else {
      path.lineTo(x, y)
    }
    path.closePath()
    path
  }
}

class GroupableControl extends Component {
  var groupBegin = false
  var groupEnd = false
  def createOutline():GeneralPath =
      GroupableControl.createOutline(
          0, 0, size.width - 1, size.height - 1, size.height / 2, groupBegin, groupEnd)
}

class GroupableLabel(var caption:String) extends GroupableControl {
  maximumSize = new Dimension(Integer.MAX_VALUE, 20)
  minimumSize = new Dimension(40, 20)
  preferredSize = new Dimension(40, 20)
  font = new Font("sans", Font.BOLD, 12)

  override def paint(g:Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

    val baseTransform = g.getTransform()
    val w = size.width - 1;
    val h = size.height - 1;
    val r = h / 2f;
    val path = createOutline();

    g.setColor(GroupableControl.fillColor);
    g.fill(path);

    g.setColor(GroupableControl.borderColor);
    g.draw(path);

    val metrics = g.getFontMetrics(font)
    var textWidth = metrics.stringWidth(caption)
    g.setFont(font)
    g.setColor(GroupableControl.detailColor);
    g.drawString(caption, (w - textWidth) / 2, (h + metrics.getAscent()) / 2)
  }
}
