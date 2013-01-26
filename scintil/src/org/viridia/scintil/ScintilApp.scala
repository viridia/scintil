package org.viridia.scintil

import scala.swing._
import scala.swing.event.{ Key, SelectionChanged }
import java.awt.event.{ InputEvent, KeyEvent }
import java.awt.{ Color, Toolkit }
import javax.swing.{ BorderFactory, Box, Icon, ImageIcon, SwingUtilities }
import javax.swing.KeyStroke.getKeyStroke
import org.viridia.scintil.graph.{ Node, PreviewRasterSource }
import org.viridia.scintil.ui._
import javax.swing.border.EmptyBorder

object ScintilApp extends SimpleSwingApplication {
  def top = new MainFrame {
    val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
    val width = screenSize.getWidth().toInt
    val height = screenSize.getHeight().toInt

    title = "Scintil"

    // -----------------------------------------------------------------------
    // Panels
    // -----------------------------------------------------------------------

    var catalogExpansion = new BorderPanel() {
      minimumSize = new Dimension(20, 20)
      preferredSize = new Dimension(200, 20)
      var expand = new Button("") {
        val expandIcon = loadIcon("expand.png")
        val contractIcon = loadIcon("contract.png")
        preferredSize = new Dimension(20, 20)
        border = BorderFactory.createMatteBorder(1, 1, 1, 1, Color.GRAY)
        def setState(expanded: Boolean) {
          icon = if (expanded) contractIcon else expandIcon
        }
        setState(true)
      }
      layout(new Label("Components")) = BorderPanel.Position.West
      layout(expand) = BorderPanel.Position.East
    }

    var catalogActionPanel = new BorderPanel() {
      minimumSize = new Dimension(20, 20)
      preferredSize = new Dimension(100, 200)
      var create = new Button("Create")
      create.action = new Action("Create") {
        def apply() {
          for (nodeProto <- ComponentCatalog.selectedNodes) {
            val node = nodeProto.getClass().newInstance()
            graphView.addNode(node)
            updatePreviews()
          }
        }
      }
      layout(create) = BorderPanel.Position.North
    }

    val catalogPanel = new GridBagPanel() {
      val insets = new Insets(0, 0, 6, 0)
      def createConstraints(x: Int, y: Int, gridheight: Int = 1, weighty: Int = 0): Constraints = {
        val c = new Constraints
        c.gridx = x
        c.gridy = y
        c.gridheight = gridheight
        c.fill = GridBagPanel.Fill.Both
        c.weighty = weighty
        c.insets = insets
        return c
      }

      border = BorderFactory.createCompoundBorder(
        BorderFactory.createRaisedBevelBorder(),
        BorderFactory.createEmptyBorder(4, 2, 4, 4))
      layout(catalogExpansion) = createConstraints(0, 0)
      layout(ComponentCatalog) = createConstraints(0, 1, weighty = 4)
      layout(catalogActionPanel) = createConstraints(0, 2, weighty = 6)
    }

    val graphView = new GraphView()
    listenTo(graphView)

    val editPanel = new BorderPanel {
      minimumSize = new Dimension(300, 400)
      preferredSize = new Dimension(500, 500)
      layout(catalogPanel) = BorderPanel.Position.West
      layout(graphView.scrollPane) = BorderPanel.Position.Center
    }

    var propertyEditor = new PropertyEditor()
    val previewPanel = new NodeOutputView() {
      minimumSize = new Dimension(256, 256)
      maximumSize = new Dimension(256, 256)
      preferredSize = new Dimension(256, 256)
    }

    val propertyPanel = new BoxPanel(Orientation.Vertical) {
      minimumSize = new Dimension(256, 256)
      preferredSize = new Dimension(256, 256)
      contents += propertyEditor
      contents += previewPanel
    }
    listenTo(propertyEditor)

    val vsplit = new SplitPane(Orientation.Vertical, editPanel, propertyPanel) {}
    vsplit.resizeWeight = 1.0
    contents = vsplit
    size = new Dimension(width, height)
    propertyPanel.revalidate()
    previewPanel.revalidate()

    reactions += {
      case e: SelectionChanged => {
        propertyEditor.setNode(graphView.selectedNodes.headOption)
        previewPanel.setNode(graphView.selectedNodes.headOption)
      }

      case NodeChanged(node: Node) => {
        def markPreviewChanged(n: Node) {
          if (n.preview.rasterValid) {
            n.preview.invalidate()
            for (output <- n.output; input <- output.connectedInputs) {
              markPreviewChanged(input.node)
            }
          }
        }
        markPreviewChanged(node)
        // If any preview raster is invalid, update previews.
        if (!graphView.graph.nodes.forall(n => n.preview.rasterValid)) {
          updatePreviews()
        }
      }
    }

    val updatePreviewsTask = new Runnable() {
      val rasterSource = new PreviewRasterSource()
      override def run() {
        var visited = Set[Node]()
        def updateNodePreview(n: Node) {
          if (!(visited contains n)) {
            visited += n
            for (input <- n.inputs; source <- input.sources) {
              updateNodePreview(source)
            }
            if (!n.preview.rasterValid) {
              n.fillRaster(rasterSource, n.preview.raster)
              n.preview.rasterValid = true
              n.preview.previewImage = None
            }
          }
        }
        graphView.graph.nodes.foreach(updateNodePreview)
        graphView.repaint()
        previewPanel.repaint()
      }
    }

    def updatePreviews() {
      SwingUtilities.invokeLater(updatePreviewsTask)
    }

    // -----------------------------------------------------------------------
    // Menus
    // -----------------------------------------------------------------------

    menuBar = new MenuBar

    val shortcutKeyMask = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

    // File menu
    val fileMenu = new Menu("File") { mnemonic = Key.F }
    menuBar.contents += fileMenu

    // File > New
    fileMenu.contents += new MenuItem(new Action("New") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_N, shortcutKeyMask | InputEvent.SHIFT_MASK))
      mnemonic = KeyEvent.VK_N
      icon = loadToolbarIcon("general/New16.gif")
      def apply() = {}
    })

    // File > Open
    fileMenu.contents += new MenuItem(new Action("Open...") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_O, shortcutKeyMask))
      mnemonic = KeyEvent.VK_O
      icon = loadToolbarIcon("general/Open16.gif")
      def apply() = {}
    })

    fileMenu.contents += new Separator

    // File > Save
    fileMenu.contents += new MenuItem(new Action("Save") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_S, shortcutKeyMask))
      mnemonic = KeyEvent.VK_S
      icon = loadToolbarIcon("general/Save16.gif")
      def apply() = {}
    })

    // File > Save As...
    fileMenu.contents += new MenuItem(new Action("Save As...") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_S, shortcutKeyMask | InputEvent.SHIFT_MASK))
      mnemonic = KeyEvent.VK_A
      icon = loadToolbarIcon("general/SaveAs16.gif")
      def apply() = {}
    })

    // File > Revert
    fileMenu.contents += new MenuItem(new Action("Revert") {
      mnemonic = KeyEvent.VK_R
      def apply() = {}
    })

    fileMenu.contents += new Separator

    // File > Exit
    fileMenu.contents += new MenuItem(new Action("Exit") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_Q, shortcutKeyMask))
      mnemonic = KeyEvent.VK_X
      def apply() = {
        dispose()
        System.exit(0)
      }
    })

    // Edit menu
    val editMenu = new Menu("Edit") { mnemonic = Key.E }
    menuBar.contents += editMenu

    // Edit > Undo
    editMenu.contents += new MenuItem(new Action("Undo") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_Z, shortcutKeyMask))
      mnemonic = KeyEvent.VK_U
      icon = loadToolbarIcon("general/Undo16.gif")
      def apply() = {}
    })

    // Edit > Redo
    editMenu.contents += new MenuItem(new Action("Redo") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_Z, shortcutKeyMask | InputEvent.SHIFT_MASK))
      mnemonic = KeyEvent.VK_R
      icon = loadToolbarIcon("general/Redo16.gif")
      def apply() = {}
    })

    editMenu.contents += new Separator

    // Edit > Cut
    editMenu.contents += new MenuItem(new Action("Cut") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_X, shortcutKeyMask))
      mnemonic = KeyEvent.VK_U
      icon = loadToolbarIcon("general/Cut16.gif")
      def apply() = {}
    })

    // Edit > Copy
    editMenu.contents += new MenuItem(new Action("Copy") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_C, shortcutKeyMask))
      mnemonic = KeyEvent.VK_U
      icon = loadToolbarIcon("general/Copy16.gif")
      def apply() = {}
    })

    // Edit > Paste
    editMenu.contents += new MenuItem(new Action("Paste") {
      accelerator = Some(getKeyStroke(KeyEvent.VK_V, shortcutKeyMask))
      mnemonic = KeyEvent.VK_U
      icon = loadToolbarIcon("general/Paste16.gif")
      def apply() = {}
    })

    updatePreviews()
  }

  def loadToolbarIcon(name: String): Icon = {
    val url = getClass.getClassLoader.getResource("toolbarButtonGraphics/" + name)
    if (url != null) {
      return new ImageIcon(url)
    } else {
      return null
    }
  }

  def loadIcon(name: String): Icon = {
    val url = getClass.getResource("/rsc/icons/" + name)
    if (url != null) {
      return new ImageIcon(url)
    } else {
      return null
    }
  }
}
