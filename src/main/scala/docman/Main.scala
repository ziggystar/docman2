package docman

import java.io.{FilenameFilter, File}
import scala.swing._
import scala.swing.event.TableRowsSelected
import javax.swing.ListSelectionModel

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

object Main extends Reactor {
  val dir = new File("documents/A")
  val pdfs: Array[File] =
    dir.listFiles(new FilenameFilter { def accept(dir: File, name: String): Boolean = name.endsWith("pdf")})

  def main(args: Array[String]) {
    val docs = pdfs.map(Doc.fromFile)
    val tableModel = TModel(docs, DocProperty.ALL)

    val table = new Table with SortableTable {
      model = tableModel
      peer.getSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      peer.setAutoCreateRowSorter(true)
      (0 until peer.getColumnCount).map(peer.getColumnModel.getColumn).zip(DocProperty.ALL).foreach{case (colModel, prop) =>
        colModel.setCellEditor(prop.cellEditor)
        colModel.setCellRenderer(prop.cellRenderer)
      }
      def openSelectedPDF() {
        selection.rows.headOption.foreach{selectedPDF =>
          sys.process.Process(f"gnome-open ${tableModel.getDocAtRow(selectedPDF).pdfFile.getAbsoluteFile}").run()
        }
      }
    }

    val viewer = new PDFViewer

    this.listenTo(table.selection)
    this.reactions += {
      case TableRowsSelected(_,_,_) => viewer.setFile(Some(pdfs(table.viewToModelRow(table.selection.rows.head))))
    }

    val menuB = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Save All Meta"){tableModel.saveAllMeta()})
        contents += new MenuItem(Action("Open Selected PDF"){table.openSelectedPDF()})
      }
    }

    val frame = new MainFrame{
      menuBar = menuB
      minimumSize = new Dimension(800,600)
      contents = new SplitPane(Orientation.Vertical,new ScrollPane(table),viewer)
    }
    viewer.listenTo(table)

    frame.open()
  }
}
