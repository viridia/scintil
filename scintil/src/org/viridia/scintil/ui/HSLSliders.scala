package org.viridia.scintil.ui

import java.awt.{Color}

/**
 * Trait representing a control that contains sliders for hue, saturation, and lightness,
 * without specifying anything about how those controls are arranged.
 */
trait HSLSliders {
  val hueSlider = new GradientSlider()
  val satSlider = new GradientSlider()
  val brtSlider = new GradientSlider()

  def value:Color = {
    return new Color(Color.HSBtoRGB(hueSlider.value, satSlider.value, brtSlider.value))
  }
  def value_=(c:Color) {
    val hsb = new Array[Float](3)
    Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), hsb)
    hueSlider.value = hsb(0)
    satSlider.value = hsb(1)
    brtSlider.value = hsb(2)
  }

  protected def updateGradients() {
    val h = hueSlider.value
    val s = satSlider.value
    val b = brtSlider.value
    satSlider.setGradient(Array(
        new Color(Color.HSBtoRGB(h, 0, b)),
        new Color(Color.HSBtoRGB(h, 1, b))))
    brtSlider.setGradient(Array(
        new Color(Color.HSBtoRGB(h, s, 0)),
        new Color(Color.HSBtoRGB(h, s, .5f)),
        new Color(Color.HSBtoRGB(h, s, 1))))
  }

  protected def displayColor() {
    updateGradients()
  }

  protected def colorChanged(isAdjusting:Boolean) {
    updateGradients()
  }

  hueSlider.reactions += {
    case e:ValueChanged => colorChanged(hueSlider.isAdjusting)
  }

  satSlider.reactions += {
    case e:ValueChanged => colorChanged(satSlider.isAdjusting)
  }

  brtSlider.reactions += {
    case e:ValueChanged => colorChanged(brtSlider.isAdjusting)
  }

  hueSlider.setGradient(Array(
      Color.RED, Color.YELLOW, Color.GREEN, Color.CYAN, Color.BLUE, Color.MAGENTA, Color.RED))
}
