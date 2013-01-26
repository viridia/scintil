package org.viridia.scintil.graphics

import java.awt.Color

final class ColorFloat(var r:Float, var g:Float, var b:Float, var a:Float=1) {
  def this() = this(0, 0, 0, 1)
  def this(c:ColorFloat) = this(c.r, c.g, c.b, c.a)
  def this(c:Color) = this(
      ColorFloat.intToFloat(c.getRed()),
      ColorFloat.intToFloat(c.getGreen()),
      ColorFloat.intToFloat(c.getBlue()),
      ColorFloat.intToFloat(c.getAlpha()))

  def cpy() = new ColorFloat(this)
  def set(r:Float, g:Float, b:Float, a:Float) = {
    this.r = r; this.g = g; this
    this.b = b; this.a = a; this
  }
  def set(c:ColorFloat) = { r = c.r; g = c.g; b = c.b; a = c.a; this }
  def set(c:Color) = {
    r = ColorFloat.intToFloat(c.getRed())
    g = ColorFloat.intToFloat(c.getGreen())
    b = ColorFloat.intToFloat(c.getBlue())
    a = ColorFloat.intToFloat(c.getAlpha())
  }
  def add(c:ColorFloat) = { r += c.r; g += c.g; b += c.b; a += c.a; this }
  def add(r:Float, g:Float, b:Float, a:Float=0) = {
    this.r += r; this.g += g; this.b += b; this.a += a; this
  }
  def sub(c:ColorFloat) = { r -= c.r; g -= c.g; b -= c.b; a -= c.a; this }
  def mul(scalar:Float) = { r *= scalar; g *= scalar; b *= scalar; a *= scalar; this }
  def mul(c:ColorFloat) = { r *= c.r; g *= c.g; b *= c.b; a *= c.a; this }
  def min(c:ColorFloat) = {
    r = math.min(r, c.r);
    g = math.min(g, c.g);
    b = math.min(b, c.b);
    a = math.min(a, c.a);
    this
  }
  def max(c:ColorFloat) = {
    r = math.max(r, c.r);
    g = math.max(g, c.g);
    b = math.max(b, c.b);
    a = math.max(a, c.a);
    this
  }
  def lerp(target:ColorFloat, alpha:Float) = mul(1f - alpha).add(
      target.r * alpha,
      target.g * alpha,
      target.b * alpha,
      target.a * alpha)
  override def toString():String = "#(" + r + "," + g + "," + b + "," + a + ")"
  def toColor() = new Color(
      ColorFloat.floatToInt(r),
      ColorFloat.floatToInt(g),
      ColorFloat.floatToInt(b),
      ColorFloat.floatToInt(a))
}

object ColorFloat {
  def apply(r:Float, g:Float, b:Float, a:Float=1) = new ColorFloat(r, g, b, a)
  def apply(color:Color) = new ColorFloat(color)
  def apply() = new ColorFloat(0, 0, 0, 1)
  def floatToInt(v:Float):Int = math.round(v * 255f)
  def intToFloat(v:Int):Float = v * (1f / 255f)
}
