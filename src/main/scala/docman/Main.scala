package docman

import java.io.{FilenameFilter, File}
import scala.swing._
import scala.swing.event.TableRowsSelected

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
    val tableModel = DocumentTableModel(docs, DProp.ALL)

    val table = new DocumentTable(tableModel)

    val viewer = new PDFViewer

    this.listenTo(table.selection)
    this.reactions += {
      case TableRowsSelected(_,_,_) => table.getSelectedDocuments.headOption.foreach(d => viewer.setFile(Some(d.pdfFile)))
    }

    val menuB = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Save All Meta"){tableModel.saveAllMeta()})
        contents += new MenuItem(Action("Open Selected PDF"){
          table.getSelectedDocuments.headOption.foreach(selectedPDF =>
            sys.process.Process(f"gnome-open ${selectedPDF.pdfFile.getAbsoluteFile}").run())

        })
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
