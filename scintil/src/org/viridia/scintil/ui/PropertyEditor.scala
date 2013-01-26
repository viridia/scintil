package org.viridia.scintil.ui

import scala.swing._
import java.awt.Dimension
import java.beans.Introspector
import javax.swing.{BorderFactory, Box, JPanel, Scrollable => JScrollable}
import org.viridia.scintil.graph._

class PropertyEditor extends ScrollPane {
  abstract class ValuePresenter(val prop:NodeProperty) extends Reactor {
    def displayValue()
    def storeValue()
    reactions += {
      case e:ValueChanged => storeValue()
    }
  }
  class IntPresenter(prop:IntProperty, ed:BoundedIntEditor) extends ValuePresenter(prop) {
    def displayValue() = ed.value = prop.value
    def storeValue() {
      if (prop.value != ed.value) {
        prop.value = ed.value
        PropertyEditor.this.fireNodeChanged()
      }
    }
    this.listenTo(ed)
  }
  class FloatPresenter(prop:FloatProperty, ed:BoundedFloatEditor) extends ValuePresenter(prop) {
    def displayValue() = ed.value = prop.value
    def storeValue() {
      if (prop.value != ed.value) {
        prop.value = ed.value
        PropertyEditor.this.fireNodeChanged()
      }
    }
    this.listenTo(ed)
  }
  class ColorGradientPresenter(prop:ColorGradientProperty, ed:ColorGradientEditor)
      extends ValuePresenter(prop) {
    def displayValue() = ed.setStops(prop.value)
    def storeValue() {
      if (prop.value != ed.value) {
        prop.value = ed.toGradient
        PropertyEditor.this.fireNodeChanged()
      }
    }
    this.listenTo(ed)
  }
  class EnumerationPresenter(prop:EnumerationPropertyBase, ed:ComboBox[Enumeration#Value])
      extends ValuePresenter(prop) {
    import scala.swing.event.SelectionChanged
    def displayValue() = { ed.selection.item = prop.value }
    def storeValue() {
      if (prop.value != ed.selection.item) {
        prop.value = ed.selection.item
        PropertyEditor.this.fireNodeChanged()
      }
    }
    reactions += {
      case e:SelectionChanged => storeValue()
    }
    this.listenTo(ed.selection)
  }

  var node:Option[Node] = None
  var presenters:List[ValuePresenter] = List()

  /**
   * A scrollable panel which tightly packs its children in a vertical stack. (Odd that something
   * so simple would end up being so elaborate to implement.)
   */
  var editList = new Panel
      with SequentialContainer.Wrapper
      with Scrollable.Wrapper {
    override lazy val peer = new JPanel with SuperMixin with JScrollable {
      def getPreferredScrollableViewportSize:Dimension = getPreferredSize
      def getScrollableTracksViewportHeight:Boolean = false
      def getScrollableTracksViewportWidth:Boolean = true
      def getScrollableBlockIncrement(visRect:Rectangle, orientation:Int, direction:Int):Int = 100
      def getScrollableUnitIncrement(visRect:Rectangle, orientation:Int, direction:Int):Int = 20
    }
    final protected def scrollablePeer:JScrollable = peer
    peer.setLayout(new javax.swing.BoxLayout(peer, javax.swing.BoxLayout.Y_AXIS))
  }

  border = BorderFactory.createEmptyBorder(4, 4, 4, 4)
  minimumSize = new Dimension(200, 200)
  preferredSize = new Dimension(200, 200)
  contents = editList

  def setNode(n:Option[Node]) {
    if (node != n) {
      node = n
      refresh()
    }
  }

  def refresh() {
    var lastGroupControl:GroupableControl = null
    def setLastGroupControl(gc:GroupableControl) {
      if (gc == null && lastGroupControl != null) {
        lastGroupControl.groupEnd = true;
        editList.peer.add(Box.createVerticalStrut(4))
      }
      if (gc != null && lastGroupControl == null) {
        gc.groupBegin = true;
      }
      lastGroupControl = gc
    }
    editList.contents.clear()
    presenters = List()
    node.foreach(n => {
      for (field <- n.getClass().getDeclaredFields()) {
        field.setAccessible(true)
        val fieldType = field.getType()
        if (classOf[NodeProperty].isAssignableFrom(fieldType)) {
          val fieldVal = field.get(n).asInstanceOf[NodeProperty]
          fieldVal match {
            case prop:FloatProperty => {
              val ctrl = new ComboSliderFloat(prop.caption)
              ctrl.minimum = prop.minVal
              ctrl.maximum = prop.maxVal
              ctrl.logScale = prop.logScale
              editList.contents += ctrl
              setLastGroupControl(ctrl)
              presenters ::= new FloatPresenter(prop, ctrl)
            }
            case prop:IntProperty => {
              val ctrl = new ComboSliderInt(prop.caption)
              ctrl.minimum = prop.minVal
              ctrl.maximum = prop.maxVal
              editList.contents += ctrl
              setLastGroupControl(ctrl)
              presenters ::= new IntPresenter(prop, ctrl)
            }
            case prop:ColorGradientProperty => {
              val ctrl = new ColorGradientEditor(prop.caption)
              setLastGroupControl(null)
              editList.contents += ctrl
              presenters ::= new ColorGradientPresenter(prop, ctrl)
            }
            case prop:EnumerationPropertyBase => {
              val ctrl = new ComboBox[Enumeration#Value](prop.values)
              val label = new Label(prop.caption + " ")
              val panel = new BorderPanel {
                layout(label) = BorderPanel.Position.West
                layout(ctrl) = BorderPanel.Position.Center
              }
              setLastGroupControl(null)
              editList.contents += panel
              presenters ::= new EnumerationPresenter(prop, ctrl)
            }
            case _ => Unit
          }
        }
      }
    })
    setLastGroupControl(null)
    presenters.foreach(_.displayValue())
    editList.revalidate()
    editList.repaint()
  }

  def fireNodeChanged() {
    for (n <- node) {
      publish(new NodeChanged(n))
    }
  }
}
