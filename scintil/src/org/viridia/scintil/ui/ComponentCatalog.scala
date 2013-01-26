package org.viridia.scintil.ui

import java.awt.{Color, Dimension, SystemColor}
import javax.swing.border.{BevelBorder, CompoundBorder, EmptyBorder, SoftBevelBorder}
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import scala.collection.mutable.ArrayBuffer
import scala.swing._
import org.viridia.scintil.graph.Node
import org.viridia.scintil.generators._
import org.viridia.scintil.filters._
import javax.swing.UIManager

object ComponentCatalog extends Table {
  object Model extends AbstractTableModel {
    val columnNames = Array[String]("Type", "Component")
    var nodeList = ArrayBuffer[Node]()

    override def getColumnCount():Int = 2
    override def getRowCount():Int = nodeList.size
    override def getColumnName(col:Int) = columnNames(col)
    override def getValueAt(row:Int, col:Int):AnyRef =
        col match {
          case 0 => nodeList(row).category
          case 1 => nodeList(row).caption
        }
  }

  model = Model
  Model.nodeList.append(new PerlinNoiseGenerator)
  Model.nodeList.append(new BrickPatternGenerator)
  Model.nodeList.append(new BlendFilter)
  Model.nodeList.append(new ColorizeFilter)
  Model.nodeList.append(new MaskFilter)

  border = new SoftBevelBorder(BevelBorder.LOWERED, null, null, null, null)
  rowHeight = 20
  gridColor = Color.LIGHT_GRAY
  minimumSize = new Dimension(100, 20)
  preferredSize = new Dimension(200, 20)
  selectionForeground = UIManager.getColor("Table.foreground")
  peer.setDragEnabled(false)
  peer.setShowHorizontalLines(false)
  peer.setIntercellSpacing(new Dimension(3, 2));
  peer.getColumnModel().getColumn(0).setMaxWidth(90);
  peer.getColumnModel().setColumnMargin(4);
  peer.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);

  def selectedNodes:Option[Node] = {
    val row = peer.getSelectedRow()
    return if (row >= 0) Some(Model.nodeList(row)) else None
  }
}
