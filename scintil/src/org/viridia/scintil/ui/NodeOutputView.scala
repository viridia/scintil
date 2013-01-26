package org.viridia.scintil.ui

import scala.swing._
import org.viridia.scintil.graph._
import java.awt.RenderingHints

class NodeOutputView extends Component {
  var node:Option[Node] = None

  minimumSize = new Dimension(256, 256)
  preferredSize = new Dimension(256, 256)

  def setNode(n:Option[Node]) {
    if (node != n) {
      node = n
      repaint()
    }
  }

  override def paint(g:Graphics2D) = {
    g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    for (n <- node) {
      g.drawImage(n.preview.getImage, 0, 0, size.width, size.height, null)
    }
  }
}
