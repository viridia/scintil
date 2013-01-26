package org.viridia.scintil.math

// Mutable 2D vector class (modeled after LibGDX). Designed to conveniently
// allow consecutive mutations via method chaining.
class Vector2(var x:Float, var y:Float) {
  def this() = this(0, 0)
  def this(v:Vector2) = this(v.x, v.y)

  def cpy() = new Vector2(this)
  def len = math.sqrt(x * x + y * y).toFloat
  def len2 = x * x + y * y
  def nor() = { val m = len; if (m > 0) { x /= m; y /= m; }; this }
  def set(x:Float, y:Float) = { this.x = x; this.y = y; this }
  def set(v:Vector2) = { x = v.x; y = v.y; this }
  def add(x:Float, y:Float) = { this.x += x; this.y += y; this }
  def add(v:Vector2) = { x += v.x; y += v.y; this }
  def sub(x:Float, y:Float) = { this.x -= x; this.y -= y; this }
  def sub(v:Vector2) = { x -= v.x; y -= v.y; this }
  def mul(scalar:Float) = { this.x *= scalar; this.y *= scalar; this }
  def mul(v:Vector2) = { x *= v.x; y *= v.y; this }
  def div(scalar:Float) = { this.x /= scalar; this.y /= scalar; this }
  def div(v:Vector2) = { x /= v.x; y /= v.y; this }
  def dot(v:Vector2) = x * v.x + y * v.y
  def dst(v:Vector2) = {
    val dx = x - v.x
    val dy = y - v.y
    math.sqrt(dx * dx + dy * dy).toFloat
  }
  def dst2(v:Vector2) = {
    val dx = x - v.x
    val dy = y - v.y
    dx * dx + dy * dy
  }
  def lerp(target:Vector2, alpha:Float) = mul(1f - alpha).add(
      target.x * alpha,
      target.y * alpha)
  override def toString():String = "[" + x + "," + y + "]"
}

object Vector2 {
  def apply(x:Float, y:Float) = new Vector2(x, y)
}
