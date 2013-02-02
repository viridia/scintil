package org.viridia.scintil.graph

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.collection.JavaConverters._
import java.io.{ InputStream, IOException, OutputStream }
import com.fasterxml.jackson.core.{ JsonEncoding, JsonFactory, JsonGenerator, JsonToken }
import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import org.viridia.scintil.graphics.{ ColorFloat, ColorGradient, ColorStop }

object GraphSerializer {
  val factory = new JsonFactory()
  def serialize(g:Graph, os:OutputStream) {
    val jg = factory.createGenerator(os, JsonEncoding.UTF8);
    val nodeIds:Map[Node, Int] = g.nodes.zipWithIndex map { n => (n._1, n._2 + 1) } toMap;
    jg.useDefaultPrettyPrinter()
    jg.writeStartObject()
    jg.writeArrayFieldStart("nodes")
    for (node <- g.nodes) {
      jg.writeStartObject()
      jg.writeNumberField("id", nodeIds(node))
      jg.writeStringField("type", node.getClass.getCanonicalName())
      jg.writeNumberField("position_x", node.position.x)
      jg.writeNumberField("position_y", node.position.y)
      serializeProperties(node, jg)
      jg.writeEndObject()
    }
    jg.writeEndArray()
    jg.writeArrayFieldStart("arcs")
    for (arc <- g.arcs) {
      jg.writeStartObject()
      jg.writeNumberField("src", nodeIds(arc.source.get.node))
      jg.writeNumberField("dst", nodeIds(arc.dest.get.node))
      jg.writeStringField("term", arc.dest.get.name)
      jg.writeEndObject()
    }
    jg.writeEndArray()
    jg.writeEndObject()
    jg.close()
  }

  def serializeProperties(node:Node, jg:JsonGenerator) {
    for (field <- node.getClass().getDeclaredFields()) {
      field.setAccessible(true)
      val fieldType = field.getType()
      if (classOf[NodeProperty].isAssignableFrom(fieldType)) {
        val fieldVal = field.get(node).asInstanceOf[NodeProperty]
        fieldVal match {
          case prop:FloatProperty => jg.writeNumberField(field.getName(), prop.value)
          case prop:IntProperty => jg.writeNumberField(field.getName(), prop.value)
          case prop:ColorGradientProperty => {
            jg.writeArrayFieldStart(field.getName())
            for (stop <- prop.value.stopArray) {
              jg.writeStartArray()
              jg.writeNumber(stop.position)
              jg.writeNumber((stop.color.r * 25500f).toInt / 100f)
              jg.writeNumber((stop.color.g * 25500f).toInt / 100f)
              jg.writeNumber((stop.color.b * 25500f).toInt / 100f)
              jg.writeEndArray()
            }
            jg.writeEndArray()
          }
          case prop:EnumerationPropertyBase =>
            jg.writeStringField(field.getName(), prop.value.toString())
        }
      }
    }
  }

  def deserialize(istrm:InputStream):Graph = {
    val mapper = new ObjectMapper()
    val jp = factory.createParser(istrm);
    val root = mapper.readTree[JsonNode](jp)
    val nodes = root.get("nodes")
    val arcs = root.get("arcs")
    val nodeMap:HashMap[Int, Node] = HashMap()
    val graph = new Graph()
    graph.nodes = List()

    for (n <- nodes.asScala) {
      val id = n.get("id").asInt()
      val node = readNode(n)
      nodeMap(id) = node
      graph.addNode(node)
    }

    for (a <- arcs.asScala) {
      readArc(a, nodeMap)
    }

    graph
  }

  def readNode(jn:JsonNode):Node = {
    val typeName = jn.get("type").asText()
    val cls = this.getClass().getClassLoader().loadClass(typeName)
    if (cls == null) {
      throw new IOException("Unknown node class: " + typeName);
    }
    val node = cls.newInstance().asInstanceOf[Node]
    node.position.x = jn.get("position_x").asInt()
    node.position.y = jn.get("position_y").asInt()
    for (field <- node.getClass().getDeclaredFields()) {
      field.setAccessible(true)
      val jField = jn.get(field.getName())
      if (jField != null) {
        val fieldType = field.getType()
        if (classOf[NodeProperty].isAssignableFrom(fieldType)) {
          val fieldVal = field.get(node).asInstanceOf[NodeProperty]
          fieldVal match {
            case prop:FloatProperty => prop.value = jField.asDouble().toFloat
            case prop:IntProperty => prop.value = jField.asInt()
            case prop:ColorGradientProperty => {
              prop.value = readGradient(jField)
            }
            case prop:EnumerationPropertyBase => {
              val strval = jField.asText()
              val v = prop.values.find(_.toString() == strval)
              if (v.isEmpty) {
                throw new IOException("Invalid enumeration value: " + strval);
              }
              prop.value = v.get
            }
          }
        }
      }
    }

    node
  }

  def readArc(jn:JsonNode, nodeMap:HashMap[Int, Node]):Arc = {
    // Source node
    val srcId = jn.get("src").asInt
    if (!(nodeMap contains srcId)) {
      throw new IOException("Invalid node id: " + srcId);
    }
    val src = nodeMap(srcId)

    // Destination node
    val dstId = jn.get("dst").asInt
    if (!(nodeMap contains dstId)) {
      throw new IOException("Invalid node id: " + dstId);
    }
    val dst = nodeMap(dstId)

    // Destination terminal
    val term = jn.get("term").asText()

    // The arc
    val arc = new Arc()
    if (src != null) {
      src.output.get.connect(arc)
    }
    if (dst != null) {
      dst.inputs.find(_.name == term).foreach(_.connect(arc))
    }
    arc
  }

  def readGradient(jn:JsonNode):ColorGradient = {
    val stops:ArrayBuffer[ColorStop] = new ArrayBuffer()
    for (cs <- jn.elements().asScala) {
      val position = cs.get(0).asDouble().toFloat
      val r = cs.get(1).asDouble().toFloat / 255f
      val g = cs.get(2).asDouble().toFloat / 255f
      val b = cs.get(3).asDouble().toFloat / 255f
      stops += new ColorStop(position, ColorFloat(r, g, b))
    }
    new ColorGradient(stops.toArray)
  }
}
