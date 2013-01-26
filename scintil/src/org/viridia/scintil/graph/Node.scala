package org.viridia.scintil.graph
import java.awt.Point
import java.awt.image.{ Raster, WritableRaster }
import java.beans.Introspector

/**
 * Trait that provides a raster containing the data received on a specific input terminal.
 * The raster may be created on the fly or cached; In either case, the data in the raster
 * will be updated to reflect the most recent computation.
 */
trait RasterSource {
  def getRaster(node:Node):Raster
  def getRaster(terminal:InputTerminal):Option[Raster]
}

class PreviewRasterSource extends RasterSource {
  def getRaster(node:Node):Raster = {
    return node.preview.raster
  }
  def getRaster(terminal:InputTerminal):Option[Raster] = {
    for (arc <- terminal.arc; output <- arc.source) {
      return Some(output.node.preview.raster)
    }
    return None
  }
}

/** Companion object for class Node. */
object Node {
  val previewImageSize = 80
}

/** A node in the graph. All nodes are capable of producing an image as output. */
abstract class Node {
  val position = new Point
  val preview = new PreviewImage
  def caption:String
  def category:String

  // A node can have 0 or more input terminals
  def inputs:Seq[InputTerminal]

  // A node can have 0 or 1 output terminals
  def output:Option[OutputTerminal]

  def generatePreview(rs:RasterSource) {
    fillRaster(rs, preview.raster)
    preview.rasterValid = true
  }

  def fillRaster(rs:RasterSource, target:WritableRaster);
}

/** Trait for node types that have an output terminal */
trait HasOutput extends Node {
  protected val out = new OutputTerminal("Out", this)
  def output = Some(out)
}

/** Trait for node types that have no outputs */
trait HasNoOutput {
  def output = None
}

/** Trait for node types that have no inputs */
trait HasNoInputs {
  def inputs = List[InputTerminal]()
}

/** Trait for node types that have a single input */
trait HasOneInput extends Node {
  val input = new InputTerminal("In", this)
  def inputs = List(input)
}

/** A generator has one output and no inputs. */
abstract class Generator extends Node with HasOutput with HasNoInputs {
  override def category = "Generator"
}

/** A filter has one output and one or more inputs. */
abstract class Filter extends Node with HasOutput {
  override def category = "Filter"
}

/** A display has one input and zero outputs. */
abstract class Display extends Node with HasNoOutput with HasOneInput {
  override def category = "Display"
}
