package docman

import java.io.{FilenameFilter, File}
import scala.swing._
import scala.swing.event.TableRowsSelected
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

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

    table.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: ListSelectionEvent): Unit = {
        if(!e.getValueIsAdjusting)
          table.getSelectedDocuments.headOption.foreach(d => viewer.setFile(Some(d.pdfFile)))
      }
    })

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
      contents = new SplitPane(Orientation.Vertical,new ScrollPane(Component.wrap(table)),viewer)
    }

    frame.open()
  }
}
