package org.viridia.scintil.math

// Mutable 3D vector class (modeled after LibGDX). Designed to conveniently
// allow consecutive mutations via method chaining.
class Vector3(var x:Float, var y:Float, var z:Float) {
  def this() = this(0, 0, 0)
  def this(v:Vector3) = this(v.x, v.y, v.z)

  def cpy() = new Vector3(this)
  def len = math.sqrt(x * x + y * y + z * z).toFloat
  def len2 = x * x + y * y + z * z
  def nor() = { val m = len; if (m > 0) { x /= m; y /= m; z /= m }; this }
  def set(x:Float, y:Float, z:Float) = { this.x = x; this.y = y; this.z = z; this }
  def set(v:Vector3) = { x = v.x; y = v.y; z = v.z; this }
  def add(x:Float, y:Float, z:Float) = { this.x += x; this.y += y; this.z += z; this }
  def add(v:Vector3) = { x += v.x; y += v.y; z += v.z; this }
  def sub(x:Float, y:Float, z:Float) = { this.x -= x; this.y -= y; this.z -= z; ; this }
  def sub(v:Vector3) = { x -= v.x; y -= v.y; z -= v.z; this }
  def mul(scalar:Float) = { this.x *= scalar; this.y *= scalar; this.z *= scalar; this }
  def mul(v:Vector3) = { x *= v.x; y *= v.y; z *= v.z; this }
  def div(scalar:Float) = { this.x /= scalar; this.y /= scalar; this.z /= scalar; this }
  def div(v:Vector3) = { v.x /= x; y /= v.y; z /= v.z; this }
  def dot(v:Vector3) = x * v.x + y * v.y + z * v.z
  def dst(v:Vector3) = {
    val dx = x - v.x
    val dy = y - v.y
    val dz = z - v.z
    math.sqrt(dx * dx + dy * dy + dz * dz).toFloat
  }
  def dst2(v:Vector3) = {
    val dx = x - v.x
    val dy = y - v.y
    val dz = z - v.z
    dx * dx + dy * dy + dz * dz
  }
  def lerp(target:Vector3, alpha:Float) = mul(1f - alpha).add(
      target.x * alpha,
      target.y * alpha,
      target.z * alpha)
  override def toString():String = "[" + x + "," + y + "," + z + "]"
}
