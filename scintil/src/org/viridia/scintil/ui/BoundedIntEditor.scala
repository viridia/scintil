package org.viridia.scintil.ui

import scala.swing.{Orientable, Orientation, Reactor, Publisher}
import org.viridia.scintil.math.MathUtils

trait BoundedIntEditor extends Orientable with Publisher {
  private var _value = 0
  private var _minimum = 0
  private var _maximum = 100
  private var _isAdjusting = false
  private var _orientation = Orientation.Horizontal

  def repaint():Unit

  protected def setValue(v:Int) {
    val cv = MathUtils.clamp(v, _minimum, _maximum)
    if (cv != _value) {
      _value = cv
      repaint()
      publish(new ValueChanged)
    }
  }

  def orientation = _orientation
  def orientation_=(o:Orientation.Value) = _orientation = o;

  def value:Int = _value
  def value_=(v:Int) {
    setValue(v)
  }

  def minimum:Int = _minimum
  def minimum_=(m:Int) = {
    _minimum = math.min(m, _maximum)
    setValue(_value)
  }

  def maximum:Int = _maximum
  def maximum_=(m:Int) = {
    _maximum = math.max(m, _minimum)
    setValue(_value)
  }

  def isAdjusting:Boolean = _isAdjusting
  def isAdjusting_=(v:Boolean) = _isAdjusting = v

  protected def range:Int = math.max(1, maximum - minimum);
}
