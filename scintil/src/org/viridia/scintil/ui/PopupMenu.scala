package org.viridia.scintil.ui

import scala.swing.{Action, Component, MenuItem}
import javax.swing.JPopupMenu

class PopupMenu extends Component {
  override lazy val peer : JPopupMenu = new JPopupMenu

  def add(item:MenuItem) { peer.add(item.peer) }
  def add(action:Action) { add(new MenuItem(action)) }
  def +=(item:MenuItem): this.type = { peer.add(item.peer); this }
  def +=(action:Action): this.type = { add(action); this }

  def setVisible(visible:Boolean) { peer.setVisible(visible) }
  def show(component:Component, x:Int, y:Int) = peer.show(component.peer, x, y)
}
