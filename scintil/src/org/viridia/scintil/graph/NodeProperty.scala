package org.viridia.scintil.graph

import org.viridia.scintil.graphics.ColorGradient

/** Used to represent a settable attribute of a node. */
class NodeProperty(val caption:String) {}

/** Node property for integer scalar values. */
class FloatProperty(
    caption:String,
    var value:Float=0,
    val minVal:Float=Float.MinValue,
    val maxVal:Float=Float.MaxValue,
    val logScale:Boolean=false)
    extends NodeProperty(caption) {
}

/** Node property for float scalar values. */
class IntProperty(
    caption:String,
    var value:Int=0,
    val minVal:Int=Int.MinValue,
    val maxVal:Int=Int.MaxValue)
    extends NodeProperty(caption) {
}

/** Node property for color gradients. */
class ColorGradientProperty(caption:String, var value:ColorGradient = new ColorGradient())
    extends NodeProperty(caption) {
}

///** Node property for enumerations. */
//class EnumerationProperty(caption:String, var value:Enumeration.Value)
//    extends NodeProperty(caption) {
//}
