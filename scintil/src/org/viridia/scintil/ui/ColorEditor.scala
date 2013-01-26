package org.viridia.scintil.ui

import java.awt.{Color, Dimension, Insets}
import javax.swing.BorderFactory
import scala.swing.{GridBagPanel, Panel, Publisher}
import scala.swing.event.{Event}
import org.viridia.scintil.graphics.ColorFloat

case class ColorChanged() extends Event

class ColorEditor extends GridBagPanel with Publisher with HSLSliders {
  border = BorderFactory.createDashedBorder(Color.BLUE)
  val insets = new Insets(4, 4, 0, 0)
  val color = new ColorFloat()
  layout(hueSlider) = createConstraints(0, 0, weightx=4)
  layout(satSlider) = createConstraints(0, 1, weightx=4)
  layout(brtSlider) = createConstraints(0, 2, weightx=4)
  val preview = new Panel() {
    border = BorderFactory.createLoweredBevelBorder()
  }
  layout(preview) = createConstraints(1, 0, gridheight=3, weightx=1)

  maximumSize = new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE)

  def createConstraints(x:Int, y:Int, gridheight:Int = 1, weightx:Int = 0):Constraints = {
    val c = new Constraints
    c.gridx = x
    c.gridy = y
    c.gridheight = gridheight
    c.fill = GridBagPanel.Fill.Both
    c.weightx = weightx
    c.insets = insets
    return c
  }

  override def displayColor() {
    super.displayColor()
    preview.background =
        new Color(Color.HSBtoRGB(hueSlider.value, satSlider.value, brtSlider.value))
  }

  displayColor()
}
