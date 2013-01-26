package org.viridia.scintil.ui

import scala.swing.{Orientable, Orientation, Reactor, Publisher}
import org.viridia.scintil.math.MathUtils

trait BoundedFloatEditor extends Orientable with Publisher {
  private var _value = 0f
  private var _minimum = 0f
  private var _maximum = 1f
  private var _isAdjusting = false
  private var _orientation = Orientation.Horizontal

  def repaint():Unit

  protected def setValue(v:Float) {
    val cv = MathUtils.clamp(v, _minimum, _maximum)
    if (cv != _value) {
      _value = cv
      repaint()
      publish(new ValueChanged)
    }
  }

  def orientation = _orientation
  def orientation_=(o:Orientation.Value) = _orientation = o;

  def value:Float = _value
  def value_=(v:Float) {
    setValue(v)
  }

  def minimum:Float = _minimum
  def minimum_=(m:Float) = {
    _minimum = math.min(m, _maximum)
    setValue(_value)
  }

  def maximum:Float = _maximum
  def maximum_=(m:Float) = {
    _maximum = math.max(m, _minimum)
    setValue(_value)
  }

  def isAdjusting:Boolean = _isAdjusting
  def isAdjusting_=(v:Boolean) = _isAdjusting = v

  protected def range:Float = math.max(1, maximum - minimum);
}

